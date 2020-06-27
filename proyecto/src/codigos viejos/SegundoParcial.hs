module SegundoParcial

    ( 
    )
    where
        import Data.List 
        import Data.Maybe

        data Month =  Enero | Febrero |  Marzo | Abril | Mayo | Junio | Julio | Agosto | Setiembre | Octubre | Noviembre | Diciembre 
            deriving (Eq , Show, Enum)

        nextMonth :: Month -> Month
        nextMonth Diciembre = Enero
        nextMonth mes = succ mes

        previousMonth :: Month -> Month
        previousMonth Enero = Diciembre
        previousMonth mes = pred mes 

        months :: Month -> Int -> Month
        months mes n
            |n==0 = mes
            |n==1 = nextMonth mes
            |n==(-1) = previousMonth mes
            |n>1 = months (nextMonth mes) (con - 1)
            |n<(-1) = months (previousMonth mes) (con + 1)
                where
                    con =  n `mod` 12

        type RGBs = (Int, Int, Int)

        data Colour = RGB RGBs | Green | Red | Blue | White | Black 
            deriving(Show, Eq)
        
        values:: Colour -> RGBs
        values Green = (0,255,0)
        values Red   = (255,0,0)
        values Blue  = (0,0,255)
        values White = (255,255,255)
        values Black = (0,0,0)
        values (RGB tripleta) = tripleta

        red :: Colour -> Int
        red color = x
            where
                (x,y,z) = values color
        
        green :: Colour -> Int
        green color = y
            where
                (x,y,z) = values color
        
        blue :: Colour -> Int
        blue color = z
            where
                (x,y,z) = values color

----------------------------------------------------------------------------------------------------------------
        data Tree a = Node a (Tree a)(Tree a)| Empty
            deriving(Eq,Show,Ord)

        inOrder :: Tree a -> [a]
        inOrder Empty = []
        inOrder (Node a Empty Empty) = [a]
        inOrder (Node x tl t2) = (inOrder tl) ++ [x] ++ (inOrder t2)

        preOrder :: Tree a -> [a]
        preOrder Empty = []
        preOrder (Node a Empty Empty) = [a]
        preOrder (Node x tl t2) = [x] ++ (preOrder tl) ++ (preOrder t2)

        postOrder :: Tree a -> [a]
        postOrder Empty = []
        postOrder (Node a Empty Empty) = [a]
        postOrder (Node x tl t2) = (postOrder tl) ++ (postOrder t2) ++ [x]


        binTreeSearch :: (Ord a) => (Tree a) -> a -> Bool        
        binTreeSearch (Node valor Empty Empty) busc = valor == busc
        binTreeSearch Empty _ = False
        binTreeSearch (Node a izq der) valor
            |valor<a = binTreeSearch izq valor
            |valor>a = binTreeSearch der valor
            |valor==a = True

        isSortedTree :: (Ord a) => (Tree a) -> Bool
        isSortedTree arbol = isSorted lista True
            where
                lista = inOrder arbol
        
        isSorted :: (Ord a) => [a] -> Bool -> Bool
        isSorted [] condicion = True
        isSorted [_] condicion = True 
        isSorted (primero:segundo:resto) True
            |primero < segundo = isSorted(segundo:resto) True
            |primero > segundo = False
        isSorted (primero:segundo:resto) False
            |primero > segundo = isSorted(segundo:resto) False
            |primero < segundo = False
            

        binTreeInsert :: (Ord a) => (Tree a) -> a -> (Tree a)
        binTreeInsert Empty valor = (Node valor Empty Empty)
        binTreeInsert (Node valor izq der) agre
            |valor>agre = (Node valor (binTreeInsert izq agre) der)
            |valor<agre = (Node valor izq (binTreeInsert der agre))
            |valor==agre = (Node valor izq der)

        data Numerical = IntNum Int | DoubleNum Double
        instance Show Numerical where
            show (IntNum qty) = (show qty) 
            show (DoubleNum qty) = (show qty) 
            

        instance Eq Numerical where
            (IntNum qty) == (DoubleNum qty2) = ((fromIntegral qty ) == qty2 )
            (DoubleNum qty) == (IntNum qty2) = (qty == (fromIntegral qty2 )) 
            (IntNum qty) == (IntNum qty2) = (qty == qty2) 
            (DoubleNum qty) == (DoubleNum qty2) = ( qty) == ( qty2 )

        
        --importar Data.Maybe
        filterMap :: (Eq b) => (a -> Maybe b) -> [a] -> [b]
        filterMap func (l:ls) --si se puede dividir cabeza-cola
            | func l == Nothing = filterMap func ls
            | otherwise = [fromJust (func l)] ++ filterMap func ls
        filterMap _ [] = [] --si no se puede dividir cabeza-cola


        data BinTree a = TreeNode a (BinTree a) (BinTree a) | EmptyTree deriving (Show, Eq)

        --Se desea una función similar a zipWith pero para BinTree. 
        zipTree:: (a->a->b) -> BinTree a -> BinTree a -> BinTree b
        zipTree  _ EmptyTree _  = EmptyTree
        zipTree  _ _ EmptyTree = EmptyTree
        zipTree func (TreeNode a left1 right1) (TreeNode b left2 right2) = (TreeNode (func a b) (zipTree func left1 left2) (zipTree func right1 right2))