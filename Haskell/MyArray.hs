-- This program was written by Franciszek Jemioło identified by index number
-- 346919.
module MyArray 
( range
, index
, inRange
, rangeSize
, Array
, listArray
, (!)
, elems
, array
, update
, (//)) where
import Prelude
----------------------------INDEXES---------------------------------------------

class Ord a => Ix a where
    -- range (lo,hi) gives a list of all indexes
    range :: (a, a) -> [a]
    -- index (lo,hi) i gives next number of index i in range
    -- (from 0)
    -- i.e. index (7,9) 8 = 1; index ('a', 'd') 'd' = 3
    -- error message if index is out of range
    index :: (a, a) -> a -> Int
    inRange :: (a, a) -> a -> Bool
    rangeSize :: (a, a) -> Int


instance Ix Char where
    range (a,b) = [toEnum x | x <- [(fromEnum a)..(fromEnum b)]]

    index (a,b) c = if ((fromEnum a <= fromEnum c) && 
        (fromEnum b >= fromEnum c)) then 
        fromEnum c - fromEnum a else error "Index out of range!"

    inRange (a, b) c = if ((fromEnum a <= fromEnum c) && 
        (fromEnum b >= fromEnum c)) then 
        True else False

    rangeSize (a, b) = if (fromEnum a <= fromEnum b) then 
        fromEnum b - fromEnum a + 1 else 0


instance Ix Int where
    range (a,b) = [a..b]

    index (a,b) c = if ((a <= c) && (b >= c)) then (c - a) else 
        error "Index out of range!"

    inRange (a, b) c = if ((a <= c) && (b >= c)) then True else False

    rangeSize (a, b) = if (a <= b) then ((b - a) + 1) else 0


instance Ix Integer where
    range (a,b) = [toEnum x | x <- [(fromEnum a)..(fromEnum b)]]

    index (a,b) c = if ((fromEnum a <= fromEnum c) && 
        (fromEnum b >= fromEnum c)) then 
        fromEnum c - fromEnum a else error "Index out of range!"

    inRange (a, b) c = if ((fromEnum a <= fromEnum c) && 
        (fromEnum b >= fromEnum c)) then 
        True else False

    rangeSize (a, b) = if (fromEnum a <= fromEnum b) then 
        fromEnum b - fromEnum a + 1 else 0 --error "Empty range!"


instance (Ix a, Ix b) => Ix (a,b) where
    range ((x, y), (c, d)) = [(n, m) | n <- range (x, c), m <- range (y, d)]

    index ((x, y), (c, d)) (k, l) = if ((inRange (x, c) k) && 
            (inRange (y, d) l)) 
        then ((index (x, c) k) * (rangeSize (x, c)) + index (y, d) l) 
        else error "Index out of range"

    inRange ((x, y), (c, d)) (k, l) = (inRange (x, c) k) && (inRange (y, d) l)

    rangeSize ((x, y), (c, d)) = (rangeSize (x, c)) * (rangeSize (y, d))


----------------------------ARRAY-----------------------------------------------

-- Declaring AVL tree data type
-- Node (key,value) Left Right BalanceFactor
data AVLTree i e = Nil | Node (i,e) (AVLTree i e) (AVLTree i e) Int


indexValue (Node (i, e) _ _ _) = i
balance Nil = (-1)
balance (Node _ _ _ br) = br

instance (Eq i) => Eq (AVLTree i e) where
    Nil == Nil = True
    (Node x l r br) == Nil = False
    Nil == (Node x l r br) = False
    (Node (x1, y1) l r br1) == (Node (x2, y2) l1 r1 br2) = 
        (x1 == x2) && (l == l1) && (r == r1)

-- Depth of a tree -- no longer needed
depth Nil = 0
depth (Node (i, e) l r _) = (max (depth l) (depth r)) + 1

-- Returns content of tree in order as list 
toList :: (Ix i) => AVLTree i e -> [e]
toList Nil = []
toList (Node (i, e) l r _) = (toList l) ++ [e] ++ (toList r)

-- Returns node balance factor
balanceFactor :: (Ix i) => AVLTree i e -> AVLTree i e -> Int
balanceFactor l r = (balance r) - (balance l)

-- Search - returning maybe int
search :: (Ix i) => AVLTree i e -> i -> Maybe e
search Nil s = Nothing
search (Node (i, e) l r _) s
    | i == s = Just e
    | i < s = (search r s)
    | i > s = (search l s)

-- Rotations on tree
balanceLL (Node (i, e) (Node (i1, e1) l1 r1 br1) r br) = 
    (Node (i1, e1) l1 (Node (i, e) r1 r (br + 1)) (br1 + 1))

balanceLR (Node (i, e) (Node (i1, e1) l1 
    (Node (i1r, e1r) r1l r1r br2) br1) r br) =
    (Node (i1r, e1r) (Node (i1, e1) l1 r1l (br1 - 1)) 
        (Node (i, e) r1r r (br + 2)) (br2))

balanceRL (Node (i, e) l (Node (ir, er) 
    (Node (ir1, er1) lr1 rr1 br2) rr br1) br) =
    (Node (ir1, er1) (Node (i, e) l lr1 (br - 2)) 
        (Node (ir, er) rr1 rr (br1 + 1)) br2)

balanceRR (Node (i, e) l (Node (ir, er) lr rr br1) br) = 
    (Node (ir, er) (Node (i, e) l lr (br - 1)) rr (br1 - 1))

-- Balanced insert
insert :: (Ix i) => AVLTree i e -> (i, e) -> AVLTree i e
insert Nil (i, e) = (Node (i, e) Nil Nil 0)
insert (Node (i, e) l r br) (x, el)
    | x == i = (Node (i, el) l r br)
    | (x < i) && (l == Nil) = (Node (i, e) (Node (x, el) Nil Nil 0) r (br - 1))
    | (x > i) && (r == Nil) = (Node (i, e) l (Node (x, el) Nil Nil 0) (br + 1))
    | (x < i) && ((br - 1)== -2) && (x < indexValue l) = 
        balanceLL (Node (i, e) li r (br - 1))
    | (x < i) && ((br -1) == -2) && (x > indexValue l) = 
        balanceLR (Node (i, e) li r (br - 1))
    | (x > i) && ((br + 1) == 2) && (x < indexValue r) = 
        balanceRL (Node (i, e) l ri (br + 1))
    | (x > i) && ((br + 1) == 2) && (x > indexValue r) = 
        balanceRR (Node (i, e) l ri (br + 1))
    | x < i = (Node (i, e) li r (br - 1))
    | x > i = (Node (i, e) l ri (br + 1))
        where li = insert l (x, el)
              ri = insert r (x, el)


fromList :: (Ix i) => [(i, e)] -> AVLTree i e
fromList [] = Nil
fromList xs = foldl insert Nil xs


-- Array contains lower bound, upper bound, and AVL tree of 
-- elements paired with indexes
data Array i e = Array {  lo :: i
                        , hi :: i
                        , elements :: AVLTree i e
                       }


-- Creates array for given range and from given list of elements
-- Undefined behaviour when range is smaller than given list.
listArray :: (Ix i) => (i, i) -> [e] -> Array i e
listArray (a, b) xs =
    Array {
      lo=a
    , hi=b
    , elements=fromList (zip (range (a,b)) xs)
    }


-- Returns element in array of given index
(!) :: (Ix i) => Array i e -> i -> e
(!) arr idx = if (inRange (lo arr, hi arr) idx)
    then
        let
            result = search (elements arr) idx
        in
            case result of
                Nothing -> undefined
                Just e -> e
    else error "Index out of range!"


-- Returns list of elements in array (in index ordering)
elems :: (Ix i) => Array i e -> [e]
elems arr = toList $ elements arr


-- Creates array from given list of pairs (index, value)
-- Undefined behaviour when given list is bigger than range
array :: (Ix i) => (i, i) -> [(i, e)] -> Array i e
array (a, b) xs = 
    Array {
      lo=a
    , hi=b
    , elements=fromList xs
    }


-- Returns array updated on given index
update :: (Ix i) => i -> e -> Array i e -> Array i e
update idx el arr = if (inRange (lo arr, hi arr) idx)
    then
        Array {
          lo=lo arr
        , hi=hi arr
        , elements=insert (elements arr) (idx, el)
        }
    else error "Index out of range!"


-- Returns array changed on given list of indexes
(//) :: (Ix i) => Array i e -> [(i, e)] -> Array i e
(//) arr [] = arr
(//) arr ((x, y):xs) = update x y w where w = (//) arr xs 

