module PList (
    Addr,
    Node (Node, this, prev),
    List (List, active, store),
    singleton,
    get,
    getActive,
    upd,
    updActive,
    push,
    pop,
) where

type RList a = ([a], Int)

(@) :: RList a -> Addr -> a
(xs, n) @ i = xs !! (n - i - 1)

cons :: a -> RList a -> RList a
cons x (xs, n) = (x : xs, n + 1)

updAt :: Int -> (a -> a) -> RList a -> RList a
updAt i f (xs, n) = (upd f (n - i - 1) xs, n)
  where
    upd f 0 (x : xs) = f x : xs
    upd f n (x : xs) = x : upd f (n - 1) xs

count :: RList a -> Int
count = snd

single :: a -> RList a
single x = ([x], 1)

type Addr = Int

data Node a = Node
    { prev :: Maybe Addr
    , this :: a
    }
    deriving (Eq, Ord, Show)

data List a = List
    { active :: Addr
    , store :: RList (Node a)
    }
    deriving (Eq, Ord, Show)

singleton :: a -> List a
singleton x =
    List
        { active = 0
        , store = single Node{prev = Nothing, this = x}
        }

get :: Addr -> List a -> Node a
get i l = store l @ i

getActive :: List a -> Node a
getActive l = get (active l) l

upd :: Addr -> (a -> a) -> List a -> List a
upd i f l = l{store = updAt i f' $ store l}
  where
    f' (Node prev this) = Node prev (f this)

updActive :: (a -> a) -> List a -> List a
updActive f l = upd (active l) f l

push :: a -> List a -> List a
push x (List active store) =
    List
        { active = count store
        , store = cons Node{prev = Just active, this = x} store
        }

pop :: List a -> Maybe (List a)
pop l = do
    parent <- prev $ store @ active
    pure l{active = parent}
  where
    List active store = l
