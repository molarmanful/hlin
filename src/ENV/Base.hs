{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ENV.Base where

import ANY
import Control.Monad
import Control.Monad.Except
import Control.Monad.State (MonadState, StateT, execStateT, get, put)
import Data.Foldable (toList)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import GHC.Conc (atomically)
import Optics
import Optics.State.Operators ((%=), (.=))
import qualified StmContainers.Map as CM
import Types
import Util

instance Show ENV where
  show = unlines . map show . toList . view #stack

run :: String -> IO ENV
run s =
  dENV >>= unENVS do
    loadLines ls
    #code .= parse ls
    loop
  where
    ls = lines s

loadLines :: [String] -> ENVS ()
loadLines = flip
  zipWithM_
  [0 ..]
  \i v -> use #lns >>= setCM (LINE (v, Nothing)) (PATH ("", i))

loop :: ENVS ()
loop =
  use #code >>= \case
    [] -> return ()
    x : xs -> do
      #code .= xs
      choice x
      loop

choice :: ANY -> ENVS ()
choice (CMD a) = cmd a
choice a = push a

cmd :: String -> ENVS ()
cmd ('\\' : k@(_ : _)) = do
  path <- use #path
  push $ toFN path $ CMD k
cmd (stripPrefix "=$$" -> Just k@(_ : _)) = arg1 (setgvar k)
cmd (stripPrefix "=$" -> Just k@(_ : _)) = arg1 (setvar k)
cmd c@(stripPrefix "$$" -> Just k@(_ : _)) = pushgvar c k
cmd c@('$' : k@(_ : _)) = pushvar c k
cmd ('#' : _ : _) = return ()
cmd "[" = do
  stack <- use #stack
  arr <- use #arr
  #stack .= Seq.empty
  #arr .= stack : arr
cmd c = evalvar c c

cmd' :: String -> ENVS ()
cmd' x = case HM.lookup x cmds of
  Nothing -> throwError $ "\"" ++ x ++ "\" not found"
  Just a -> a

cmds :: HashMap String (ENVS ())
cmds =
  HM.fromList
    [ -- vars
      ("UN", push UN),
      ("$T", push $ TF True),
      ("$F", push $ TF False),
      ("()", push UN >> cmd ">F"),
      ("[]`", push $ toSEQ UN),
      ("[]", push $ toARR UN),
      ("{}", push $ toMAP UN),
      ( "$L",
        do
          PATH (_, n) <- use #path
          push $ NUM $ fromIntegral n
      ),
      ("$PI", push pi),
      ("$E", push $ exp 1),
      ("$W", push $ SEQ [0 ..]),
      ("$N", push $ SEQ [1 ..]),
      -- conversion
      (">?", mod1 toTF),
      (">>?", modv1 toTF),
      (">N", mod1 toNUM),
      (">>N", modv1 toNUM),
      (">N%", mods1 $ pair . properFraction),
      (">I", mod1 toINT),
      (">>I", modv1 toINT),
      (">R", mod1 toRAT),
      (">>R", modv1 toRAT),
      (">S", mod1 toSTR),
      (">>S", modv1 toSTR),
      (">F", use #path >>= mod1 . toFN),
      (">>F", use #path >>= modv1 . toFN),
      (">Q", mod1 toSEQ),
      (">>Q", modv1 toSEQ),
      (">A", mod1 toARR),
      (">>A", modv1 toARR),
      (">M", mod1 toMAP),
      (">>M", modv1 toMAP),
      (",", mod2 \a b -> SEQ [a, b]),
      (",,", mod1 \a -> SEQ [a]),
      (",`", #stack %= Seq.singleton . seqtoARR),
      (",_", arg1 $ pushs . seqfromARR . toARR),
      (",,_", arg1 $ (#stack .=) . seqfromARR . toARR),
      ("\\", cmd ",," >> cmd ">F"),
      ( "'",
        modM2 \a -> vecM1 \f -> do
          ENV {stack} <- evalSt f $ seqfromARR a
          return $ seqtoARR stack
      ),
      ("'_", arg1 \a -> cmd' ",`" >> push a >> cmd' "Q" >> cmd' ",,_"),
      -- flow
      ("end", #code .= []),
      ( ".",
        do
          code <- use #code
          path <- use #path
          case code of
            [] -> cmd ";"
            c : cs -> do
              #code .= cs
              case c of
                STR a -> push $ STR $ unescText a
                a@(CMD _) -> mod1 \f -> FN path [f, a]
                _ -> push c
      ),
      ( "[",
        do
          stack <- use #stack
          arr <- use #arr
          #arr .= stack : arr
          cmd "clr"
      ),
      ( "]`",
        do
          stack <- use #stack
          arr <- use #arr
          case arr of
            [] -> return ()
            x : xs -> do
              #arr .= xs
              #stack .= x |> SEQ (toList stack)
      ),
      ("]", cmd "]`" >> cmd ">A"),
      ("#", arg1 eval),
      ("*#", arg2 \f n -> timesM (toInt n) $ eval f),
      ("&#", cmd "swap" >> cmd ">?" >> cmd "*#"),
      ("|#", cmd "swap" >> cmd "!`" >> cmd "*#"),
      ("Q", modMv1 evalQ),
      ("@@", arg1 $ evalLn . toInt),
      ("@~", cmd "$L" >> cmd "+" >> cmd "@@"),
      ("@", push 0 >> cmd "@~"),
      (";", push 1 >> cmd "@~"),
      (";;", push (-1) >> cmd "@~"),
      -- I/O
      ("i>", liftIO getLine >>= push . STR . T.pack),
      (">o", arg1 $ liftIO . putStr . toStr),
      ("n>o", arg1 $ liftIO . putStrLn . toStr),
      ("f>o", arg1 $ liftIO . print),
      -- stack
      ("dup", mods1 \a -> [a, a]),
      ("pop", mods1 $ const []),
      ("swap", mods2 \a b -> [b, a]),
      ("rot", mods3 \a b c -> [b, c, a]),
      ("rot_", mods3 \a b c -> [c, a, b]),
      ("dupd", mods2 \a b -> [a, a, b]),
      ("over", mods2 \a b -> [a, b, a]),
      ("ddup", mods2 \a b -> [a, b, a, b]),
      ("edup", mods3 \a b c -> [a, b, c, a, b, c]),
      ("nip", mod2 $ const id),
      ("ppop", mods2 \_ _ -> []),
      ("qpop", mods3 \_ _ _ -> []),
      ("swapd", mods3 \a b c -> [b, a, c]),
      ("tuck", mods2 \a b -> [b, a, b]),
      ("dups", use #stack >>= push . seqtoARR),
      ("clr", #stack .= Seq.empty),
      ("rev", #stack %= Seq.reverse),
      ("pick", modM1 \a -> checkStL a $ use #stack <&> getSt a),
      ( "nix",
        arg1 \a ->
          checkStL a $
            #stack %= \s -> flip Seq.deleteAt s $ iinv (toInt a) s
      ),
      ( "trade",
        arg1 \a -> checkStL a do
          push a >> cmd "roll_"
          push (a - 1) >> cmd "roll"
      ),
      ( "roll",
        arg1 \a -> checkStL a do
          push a >> cmd "nix"
          use #stack >>= push . getSt a
      ),
      ( "roll_",
        arg1 \a -> checkStL a do
          push a >> cmd "trade"
          use #stack >>= push . getSt 0
      ),
      ("dip", arg2 \a f -> evalE f >> push a),
      -- logic
      ("!", cmd ">>?" >> modv1 (fTF1 not)),
      ("!`", cmd ">?" >> cmd "!"),
      ("&", modv2 min),
      ("&`", mod2 min),
      ("|", modv2 max),
      ("|`", mod2 max),
      ("<=>", modv2 \a b -> INT $ fromCmp $ compare a b),
      ("<=>`", mod2 \a b -> INT $ fromCmp $ compare a b),
      ("=", modv2 $ fTF2' (==)),
      ("=`", mod2 $ fTF2' (==)),
      ("<", modv2 $ fTF2' (<)),
      ("<`", mod2 $ fTF2' (<)),
      (">", modv2 $ fTF2' (>)),
      (">`", mod2 $ fTF2' (>)),
      ("<=", modv2 $ fTF2' (<=)),
      ("<=`", mod2 $ fTF2' (<=)),
      (">=", modv2 $ fTF2' (>=)),
      (">=`", mod2 $ fTF2' (>=)),
      -- math
      ("|_", modv1 floor),
      ("|~", modv1 round),
      ("|^", modv1 ceiling),
      ("_", modv1 negate),
      ("__", modv1 $ fSTR1 T.reverse),
      ("+", modv2 (+)),
      ("++", modv2 (<>)),
      ("+`", mod2 (<>)),
      ("-", modv2 (-)),
      ("--", modv2 $ fSTR2 (`T.replace` "")),
      ("*", modv2 (*)),
      ("**", modv2 $ \a b -> STR $ T.replicate (toInt b) $ toSTRW a),
      ("*`", mod2 $ flip atimes),
      ("/", modv2 (/)),
      ("/~", modv2 div),
      ("%", modv2 afmod),
      ("%~", modv2 afrem),
      ("%%", undefined),
      ("^", modv2 apow),
      ("abs", modv1 abs),
      ("+-", modv1 signum),
      ("E", push 10 >> cmd "swap" >> cmd "^"),
      ("e^", modv1 exp),
      ("logN", modv2 $ flip logBase),
      ("log", push 10 >> cmd "logN"),
      ("ln", modv1 log),
      ("sin", modv1 sin),
      ("cos", modv1 cos),
      ("tan", modv1 tan),
      ("sin_", modv1 asin),
      ("cos_", modv1 acos),
      ("tan_", modv1 atan),
      ("sinh", modv1 sinh),
      ("cosh", modv1 cosh),
      ("tanh", modv1 tanh),
      ("sinh_", modv1 asinh),
      ("cosh_", modv1 acosh),
      ("tanh_", modv1 atanh),
      -- lazy
      ("tk", undefined),
      ("dp", undefined),
      ("rep", modv1 $ SEQ . repeat),
      ("cyc", mod1 $ fSEQ1 cycle)
    ]

-- convenience

dENV :: IO ENV
dENV = do
  lns <- liftIO CM.newIO
  gscope <- liftIO CM.newIO
  gids <- liftIO CM.newIO
  return
    ENV
      { lns,
        code = [],
        path = PATH ("", 0),
        stack = Seq.empty,
        scope = HM.empty,
        gscope,
        ids = HM.empty,
        gids,
        arr = []
      }

eval :: ANY -> ENVS ()
eval (FN p a) =
  do
    env <- get
    let e =
          env
            & #code .~ a
            & #path .~ p
    use #code >>= \case
      [] -> put e
      _ -> evalScoped e env >>= put
eval a = do
  path <- use #path
  eval $ toFN path a

evalE :: ANY -> ENVS ()
evalE a = evalE' a >>= put

evalQ :: ANY -> ENVS ANY
evalQ a = evalE' a <&> fromMaybe UN . (^? #stack % _last)

evalE' :: (MonadState ENV m, MonadIO m) => ANY -> m ENV
evalE' f = use #stack >>= evalSt f

evalSt :: (MonadState ENV m, MonadIO m) => ANY -> Seq ANY -> m ENV
evalSt (FN p f) stack = do
  env <- get
  flip evalScoped env $
    env
      & #code .~ f
      & #path .~ p
      & #stack .~ stack
evalSt a s = do
  path <- use #path
  evalSt (toFN path a) s

evalScoped :: MonadIO m => ENV -> ENV -> m ENV
evalScoped e' e = do
  ENV {stack} <- eval' e'
  return $ e & #stack .~ stack

eval' :: MonadIO m => ENV -> m ENV
eval' = liftIO . unENVS loop

evalLn :: Int -> ENVS ()
evalLn n = do
  l <- fnLn n
  case l of
    Nothing -> return ()
    Just (LINE (_, a)) -> mapM_ eval a

fnLn :: Int -> ENVS (Maybe LINE)
fnLn n = do
  ENV {lns, path = PATH (fp, _)} <- get
  l <- getLn n
  case l of
    Nothing -> return Nothing
    Just (LINE (a, Nothing)) -> do
      setCM l' p lns
      return $ Just l'
      where
        p = PATH (fp, n)
        l' = LINE (a, Just $ FN p $ parse [a])
    Just (LINE _) -> return l

getLn :: Int -> ENVS (Maybe LINE)
getLn n = do
  ENV {lns, path = PATH (fp, _)} <- get
  getCM (PATH (fp, n)) lns

push :: ANY -> ENVS ()
push = (#stack %=) . flip (|>)

pushs :: Seq ANY -> ENVS ()
pushs = (#stack %=) . flip (><)

setvar :: String -> ANY -> ENVS ()
setvar k v = #scope %= (at k ?~ v)

setgvar :: String -> ANY -> ENVS ()
setgvar k v = use #gscope >>= setCM v k

getvar :: String -> ENVS (Maybe ANY)
getvar k = do
  ENV {scope} <- get
  case HM.lookup k scope of
    Nothing -> getgvar k
    v@(Just _) -> return v

getgvar :: String -> ENVS (Maybe ANY)
getgvar k = use #gscope >>= getCM k

pushvar :: String -> String -> ENVS ()
pushvar = fvar getvar push

pushgvar :: String -> String -> ENVS ()
pushgvar = fvar getgvar push

evalvar :: String -> String -> ENVS ()
evalvar = fvar getvar eval

evalgvar :: String -> String -> ENVS ()
evalgvar = fvar getgvar eval

getSt :: ANY -> Seq ANY -> ANY
getSt i s = fromMaybe UN $ s ^? ix (iinv (toInt i) s)

fvar :: (t1 -> ENVS (Maybe t2)) -> (t2 -> ENVS ()) -> String -> t1 -> ENVS ()
fvar g f c k =
  g k >>= \case
    Nothing -> cmd' c
    Just v -> f v

argN :: Int -> (Seq ANY -> ENVS ()) -> ENVS ()
argN n f = do
  checkStL' n \l m -> do
    stack <- use #stack
    let (a, b) = Seq.splitAt (l - m) stack
    #stack .= a
    f b

modN :: Int -> (Seq ANY -> ANY) -> ENVS ()
modN n f = argN n $ push . f

modsN :: Int -> (Seq ANY -> [ANY]) -> ENVS ()
modsN n f = argN n $ pushs . Seq.fromList . f

modMN :: Int -> (Seq ANY -> ENVS ANY) -> ENVS ()
modMN n f = argN n $ f >=> push

arg1 :: (ANY -> ENVS ()) -> ENVS ()
arg1 = argN 1 . lsap1

arg2 :: (ANY -> ANY -> ENVS ()) -> ENVS ()
arg2 = argN 2 . lsap2

arg3 :: (ANY -> ANY -> ANY -> ENVS ()) -> ENVS ()
arg3 = argN 3 . lsap3

mod1 :: (ANY -> ANY) -> ENVS ()
mod1 = modN 1 . lsap1

mod2 :: (ANY -> ANY -> ANY) -> ENVS ()
mod2 = modN 2 . lsap2

mod3 :: (ANY -> ANY -> ANY -> ANY) -> ENVS ()
mod3 = modN 3 . lsap3

mods1 :: (ANY -> [ANY]) -> ENVS ()
mods1 = modsN 1 . lsap1

mods2 :: (ANY -> ANY -> [ANY]) -> ENVS ()
mods2 = modsN 2 . lsap2

mods3 :: (ANY -> ANY -> ANY -> [ANY]) -> ENVS ()
mods3 = modsN 3 . lsap3

modM1 :: (ANY -> ENVS ANY) -> ENVS ()
modM1 = modMN 1 . lsap1

modM2 :: (ANY -> ANY -> ENVS ANY) -> ENVS ()
modM2 = modMN 2 . lsap2

modM3 :: (ANY -> ANY -> ANY -> ENVS ANY) -> ENVS ()
modM3 = modMN 3 . lsap3

modv1 :: (ANY -> ANY) -> ENVS ()
modv1 = mod1 . vec1

modv2 :: (ANY -> ANY -> ANY) -> ENVS ()
modv2 = mod2 . vec2

modMv1 :: (ANY -> ENVS ANY) -> ENVS ()
modMv1 = modM1 . vecM1

modMv2 :: (ANY -> ANY -> ENVS ANY) -> ENVS ()
modMv2 = modM2 . vecM2

unENVS :: (Monad m) => ExceptT String (StateT s m) a -> s -> m s
unENVS f = execStateT do
  r <- runExceptT f
  case r of
    Left e -> error e
    Right a -> return a

setCM :: (MonadIO m, Hashable key) => value -> key -> CM.Map key value -> m ()
setCM v k = liftIO . atomically . CM.insert v k

getCM :: (MonadIO m, Hashable key) => key -> CM.Map key value -> m (Maybe value)
getCM k = liftIO . atomically . CM.lookup k

checkStL :: (MonadState ENV m, MonadError [Char] m) => ANY -> m b -> m b
checkStL n f = checkStL' (toInt n) \_ _ -> f

checkStL' :: (MonadState ENV m, MonadError [Char] m) => Int -> (Int -> Int -> m b) -> m b
checkStL' n f = do
  stack <- use #stack
  let l = length stack
  if l < n
    then throwError $ "stack len " ++ show l ++ " < " ++ show n
    else f l n