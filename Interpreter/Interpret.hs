-- Written by Franciszek Jemioło, index number 346919
module Interpret where

import Data.Map as M

-- Importing monads for handling our interpreting context
import Control.Monad.Trans.Reader as R
import Control.Monad.Trans.Error as E
--import Control.Monad.State.Lazy
import Control.Monad.Trans.State as S
import Control.Monad.Identity
import Control.Monad.Trans.Class
import System.IO
-- Importing data structres of my language
import AbsJam
-- Import printing
import PrintJam

-------------------INTERPRETER-STRUCTURE----------------------------------------

-- Locations are just int's
type Loc = Integer

-- We need some type to store different kind's of objects
data Object = NULL | OInt Integer | OBool Bool | OString String 
            | OList Type [Object] 
            | ODict OType Type (M.Map OType Object)

instance Show Object where
    show (OInt n) = show n
    show (OBool b) = show b
    show (OString s) = s
    show (OList t []) = show "[]"
    show (OList t (o:os)) = show (o:os)
    show (ODict t1 t2 m) = show (M.toList m)
    show (NULL) = ""

-- The data type for Function, we remeber what is the return type, what are
-- the arguments for the function call, and whether the argument should be 
-- passed as reference or as value.
data Function = Function { retType :: Type
                         , arguments :: [Arg]
                         , codeBlock :: Block } deriving Show

-- Environment to be kept in ReaderT, we keep mapping of names to current
-- location of those variables or functions in current environment.
data Env = Env { variables :: M.Map Ident Loc
               , functions :: M.Map Ident Loc }


-- Store to be kept in StateT, we keep mapping from Location to objects,
-- and from locations to function objects.
data Store = Store { objects :: M.Map Loc Object
                   , funs :: M.Map Loc Function }

mapRefs :: [(Ident, Loc)] -> M.Map Loc Object -> [(String, String)]
mapRefs [] _ = []
mapRefs ((x,y):ys) m = case M.lookup y m of
    (Just val) -> (show x, show val):(mapRefs ys m)
    (Nothing) -> (show x, show "Null"):(mapRefs ys m)

instance Show Store where
    show s = (show $ M.toList $ objects s)

-- The type of our customError that we use for return functions, and handling
-- runtime exceptions. Also for try and catch. The other parameter
-- is for string inside of which statement we got the exception.
data CustomError = VariableUndeclared String String
                 | FunctionUndeclared String String
                 | TypeError String String
                 | InternalError String String
                 | Return Object
                 | Exception Object String
                 | ZeroDivision String

instance E.Error CustomError where
    noMsg = strMsg ""
    strMsg s = InternalError s ""

instance Show CustomError where
    show (VariableUndeclared s st) = ("In statement: " ++ st ++
        " .Variable " ++ s ++ " is not declared in current scope!")
    show (FunctionUndeclared s st) = ("In statement: " ++ st ++
        ". Function " ++ s ++ " is not declared in current scope!")
    show (TypeError s st) = "In statement: " ++ st ++ " . TypeError. " ++ s
    show (InternalError s st) = ("In statement: " ++ st ++ 
        ". InternalError. " ++ s)
    show (Return obj) = "Return. " ++ (show (obj))
    show (ZeroDivision st) = ("In statement: " ++ st ++ 
        " . Zero division exception!")
    show (Exception obj st) = ("In statement: " ++ st ++ 
        " . Exception. " ++ (show (obj)))

-- Our custom monad for interpreter that has both store, env and error handling
-- and ability to perform IO.
type RES a = R.ReaderT Env (E.ErrorT CustomError (S.StateT Store IO)) a

-------------------HELPER-FUNCTIONS---------------------------------------------

-- Allocates new cell in memory for our newly created object
alloc :: M.Map Loc a -> Loc
alloc m = case M.maxViewWithKey m of
    (Just ((k,v), _)) -> (k + 1)
    Nothing -> 0

-- Alloc a number of locations from given start location.
allocMany :: Loc -> Int -> [Loc]
allocMany l 0 = []
allocMany l x = [l] ++ (allocMany (l+1) (x-1)) 

-- Free the allocated in memory object by given key - NOT USED, handled totally 
-- by ReaderT monad (and local).
free :: M.Map Loc a -> Loc -> M.Map Loc a
free m k = M.delete k m

-- Helper functions for accessing the right monad transformer.
liftToIO = lift . lift . lift
liftToState = lift . lift
liftToError = lift

-- Helper functions for pretty acessing state 
myGets x = liftToState $ gets x
myGet = liftToState $ get
myPut s = liftToState $ put s

-- Updates mapping of variable of given name to given location in environment.
updateVariable :: Ident -> Loc -> Env -> Env
updateVariable i l e = Env { variables = (M.insert i l) $ variables e
                           , functions = functions e }

-- Updates mapping of function of given name to given location in environment.
updateFunctions :: Ident -> Loc -> Env -> Env
updateFunctions i l e = Env { variables = variables e
                            , functions = (M.insert i l) $ functions e }

-- Updates mapping of arguments to location in environment
updateArguments :: [Arg] -> [Loc] -> Env -> Env
updateArguments [] [] e = e
updateArguments [] _ _ = undefined
updateArguments _ [] _ = undefined
updateArguments ((VArg t i):as) (l:ls) e = updateArguments as ls Env {
    variables = (M.insert i l) $ variables e,
    functions = functions e
}
updateArguments ((RArg t i):as) (l:ls) e  = undefined

-- Creates in store new location for objects passed as arguments to function.
updateStateArguments :: [Loc] -> [Object] -> Store -> Store
updateStateArguments [] []  s = s
updateStateArguments _ [] _ = undefined
updateStateArguments [] _ _ = undefined
updateStateArguments (l:ls) (o:os) s = updateStateArguments ls os (Store {
        objects = (M.insert l o) $ objects s,
        funs = funs s
    }) 

-- Creates a new object in store under given location for object of given type.
updateStateReference :: Ident -> Loc -> Type -> Store -> Store
updateStateReference i l t s = case t of
    (OrdType Int) -> Store { objects = (M.insert l (OInt 0)) $ objects s
                           --, references = (M.insert i l) $ references s
                           , funs = funs s }
    (OrdType Str) -> Store { objects = (M.insert l (OString "")) $ objects s
                           --, references = (M.insert i l) $ references s
                           , funs = funs s }
    (Bool) -> Store { objects = (M.insert l (OBool False)) $ objects s
                    --, references = (M.insert i l) $ references s
                    , funs = funs s }
    _ -> undefined
    
-- Is an Object of the same type as some declared Type?
isTypeOf :: Type -> Object -> Bool
isTypeOf t o = case t of
    (OrdType Int) -> case o of
         (OInt _) -> True
         _ -> False
    (OrdType Str) -> case o of
         (OString _) -> True
         _ -> False
    (Bool) -> case o of
         (OBool _) -> True
         _ -> False
    (List t) -> case o of
         (OList t1 _) -> (t1 == t)
         _ -> False
    (Dictionary kt vt) -> case o of
         (ODict kt1 vt1 _) -> (kt1 == kt) && (vt1 == vt)
         _ -> False

-- Are two objects of the same type?
equalTypes :: Object -> Object -> Bool
equalTypes o1 o2 = case o1 of
    (OInt _) -> case o2 of
        (OInt _) -> True
        _ -> False
    (OString _) -> case o2 of
        (OString _) -> True
        _ -> False
    (OBool _) -> case o2 of
        (OBool _) -> True
        _ -> False
    (OList t _) -> case o2 of
        (OList t2 _) -> (t == t2)
        _ -> False
    (ODict kt vt _) -> case o2 of
        (ODict kt2 vt2 _) -> (kt == kt2) && (vt == vt2)
        _ -> False

-- Updates value in store under given location to given object.
updateStateReferenceWithValue :: Ident -> Loc -> Object -> Store -> Store
updateStateReferenceWithValue i l o s = Store { 
    objects = (M.insert l o) $ objects s,
    funs = funs s}

-- Creates given function under given location in given store
updateStateFunctions :: Type -> Ident -> [Arg] -> Block -> Loc -> Store -> Store
updateStateFunctions t i a b l s = Store {
    objects = objects s,
    funs = (M.insert l (Function { retType = t
                                 , arguments = a
                                 , codeBlock = b })) $ funs s
}

-- Just checking type of Arg list and Object list
listIsTypeOf :: [Arg] -> [Object] -> Bool
listIsTypeOf [] [] = True
listIsTypeOf [] _ = False
listIsTypeOf _ [] = False
listIsTypeOf ((VArg t _):ts) (o:os) = (isTypeOf t o) && (listIsTypeOf ts os)
listIsTypeOf ((RArg t _):ts) (o:os) = undefined

-- Checking if function got good type arguments
checkGoodApp :: [Object] -> Function -> Bool
checkGoodApp objs f = listIsTypeOf (arguments f) objs

-- Multiple helper functions for throwing runtime exceptions

-- Helper function to hide throws
myThrowError e = liftToError $ E.throwError e

-- Helper function to hide catches
myCatchError = (R.liftCatch catchError)

-- Some null exception
raiseNullException x st = myThrowError (InternalError (
    "Null exception. " ++ (show x) ++ " points to a place that is" ++
    " not visible in current state!") st)

-- Error when changing value of object on which we iterate in for
raiseForVariableChanged x st = myThrowError (InternalError (
    "Changing value of variable on which you iterate in for loop" ++ 
    " is forbidden! Variable " ++ (show x) ++ " has changed it's value!") st)

-- Throwing error when using some undeclared variable.
raiseUndeclaredVariable x st = myThrowError (VariableUndeclared
    (show x) st)

-- Throwing error on application of function.
raiseWrongApplication x st = myThrowError (TypeError 
    ("Wrong application of function " ++ (show x)) st)

-- Throwing error when using some undeclared function.
raiseUndeclaredFunction x st = myThrowError (FunctionUndeclared 
    (show x) st)

-- Throwing type error when there is something wrong with them.
raiseTypeError msg st = myThrowError (TypeError (msg) st)

-- Division by zero
raiseZeroDivision st = myThrowError (ZeroDivision st)


-- Special case of throwing runtime error is return statement
returnObject o = myThrowError (Return (o))

-- Helper function for inserting current statement inside an error
replaceStmt er st = case er of
    (VariableUndeclared x _) -> (VariableUndeclared x st)
    (FunctionUndeclared x _) -> (FunctionUndeclared x st)
    (TypeError s _) -> (TypeError s st)
    (InternalError s _) -> (InternalError s st)
    (Return obj) -> (Return obj)
    (ZeroDivision _) -> (ZeroDivision st)
    (Exception obj _) -> (Exception obj st)

-------------------INTERPRETING-FUNCTIONS---------------------------------------

-- Evaluating multiple kinds of expressions
-- For functions to work (return call), we had to create new type (NULL)
-- because now, every statement also returns an Object, and we have to differ
-- what statement returns what.
eval :: Exp -> RES Object

-- Evaluating given variable
eval (EVar x) = do
    vars <- asks variables
    case M.lookup x vars of
        (Just loc) -> do
            objs <- myGets objects
            case M.lookup loc objs of
                (Just z) -> return z
                Nothing -> raiseNullException x (printTree (EVar x))
        Nothing -> raiseUndeclaredVariable x (printTree (EVar x))

-- Just evaluating an Int
eval (EInt x) = return (OInt x)

-- Just evaluating a Bool value
eval (ETrue) = return (OBool True)

-- Just evaluating a Bool value
eval (EFalse) = return (OBool False)

-- Just evaluating a String value
eval (EString x) = return (OString x)

-- TO DO:
eval (EFirst x) = undefined

-- TO DO:
eval (EGet x e) = undefined 

-- TO DO:
eval (ECont x) = undefined

-- TO DO:
eval (EEmpty x) = undefined

-- TO DO:
eval (ESize x) = undefined

-- TO DO:
eval (EGetAt x e) = undefined

-- Evaluating an application of function
eval (EApp x es) = do
    fs <- asks functions
    case M.lookup x fs of
        (Just loc) -> do
            functs <- myGets funs
            case M.lookup loc functs of
                (Just f) -> do
                    vals <- myCatchError (mapM eval es)
                        (\er -> myThrowError er)
                    if (checkGoodApp vals f) then do
                        -- Allocate the arguments of the functions
                        -- For now we do not differentiate between 
                        -- passed by reference and value
                        objs <- myGets objects
                        store <- myGet
                        let loc = alloc objs
                        let locs = allocMany loc (length $ arguments f)
                        myPut (updateStateArguments locs vals store)
                        val <- myCatchError 
                            (local 
                                (updateArguments (arguments f) locs) 
                                (execStmt (BStmt (codeBlock f))))
                            (\er -> case er of
                                (Return obj) -> if (isTypeOf (retType f) obj)
                                    then return obj
                                    else (raiseTypeError ("Function call has "
                                        ++ "returned object of different " ++ 
                                        "type than declared!") 
                                        (printTree (EApp x es)))
                                _ -> myThrowError er)
                        if (isTypeOf (retType f) val) then
                            return val
                        else raiseTypeError ("Function call has "
                            ++ "returned object of different " ++ 
                            "type than declared!") (printTree (EApp x es))
                    else raiseWrongApplication x (printTree (EApp x es))

                (Nothing) -> raiseNullException x (printTree (EApp x es))
        (Nothing) -> raiseUndeclaredFunction x (printTree (EApp x es))

-- Negation of an integer
eval (ENeg e) = do
    -- Passing error up
    v <- myCatchError (eval e) 
        (\er -> myThrowError er)
    case v of
        (OInt n) -> return (OInt (-n))
        _ -> raiseTypeError
            ("You are trying integer negation not on an integer value!")
            (printTree (ENeg e))

-- Negation of a bool
eval (ENot e) = do
    v <- myCatchError (eval e) 
        (\er -> myThrowError er)
    case v of
        (OBool n) -> return (OBool (not n))
        _ -> raiseTypeError
            ("You are trying boolean negation not on a boolean value!")
            (printTree (ENot e))

-- Multiplication operations
eval (EMul e1 op e2) = do
    v1 <- myCatchError (eval e1) 
        (\er -> myThrowError er)
    case v1 of
        (OInt n1) -> do
            v2 <- myCatchError (eval e2)
                (\er -> myThrowError er)
            case v2 of
                (OInt n2) -> case op of
                    Mul -> return (OInt (n1 * n2))
                    Div -> if (n2 /= 0) 
                        then
                            return (OInt (n1 `div` n2))
                        else
                            raiseZeroDivision (printTree (EMul e1 op e2))
                    Mod -> if (n2 /= 0) 
                        then
                            return (OInt (n1 `mod` n2))
                        else 
                            raiseZeroDivision (printTree (EMul e1 op e2))
                _ -> raiseTypeError ("You are trying to" ++ 
                    "mulitply something different than integer!") 
                    (printTree (EMul e1 op e2))
        _ -> raiseTypeError
            ("You are trying to mulitply something different than integer!")
            (printTree (EMul e1 op e2))

-- Additions operations
eval (EAdd e1 op e2) = do
    v1 <- myCatchError (eval e1)
        (\er -> myThrowError er)
    case v1 of
        (OInt n1) -> do
            v2 <- myCatchError (eval e2)
                (\er -> myThrowError er)
            case v2 of
                (OInt n2) -> case op of
                    Plus -> return (OInt (n1 + n2))
                    Minus -> return (OInt (n1 - n2))
                (OString s2) -> case op of
                    Plus -> return (OString ((show n1) ++ s2))
                    Minus -> raiseTypeError ("You are trying" 
                        ++ "to subtract a string from a number...")
                        (printTree (EAdd e1 op e2))
        (OString s1) -> do
            v2 <- eval e2
            case v2 of
                (OInt n2) -> case op of
                    Plus -> return (OString (s1 ++ (show n2)))
                    Minus -> raiseTypeError ("You are trying" 
                        ++ "to subtract a string from a number...")
                        (printTree (EAdd e1 op e2))
                (OString s2) -> case op of
                    Plus -> return (OString (s1 ++ s2))
                    Minus -> raiseTypeError ("You are trying" 
                        ++ "to subtract a string from a string...")
                        (printTree (EAdd e1 op e2))
        _ -> raiseTypeError ("You are trying to add a value" 
            ++ "that is neither an integer nor a string!")
            (printTree (EAdd e1 op e2))

-- Relation operations
eval (ERel e1 op e2) = do
    v1 <- myCatchError (eval e1)
        (\er -> myThrowError er)
    v2 <- myCatchError (eval e2)
        (\er -> myThrowError er)
    case v1 of
        (OInt n1) -> case v2 of
            (OInt n2) -> case op of
                Less -> return (OBool (n1 < n2))
                LessOrEq -> return (OBool (n1 <= n2))
                Greater -> return (OBool (n1 > n2))
                GrtOrEq -> return (OBool (n1 >= n2))
                Equal -> return (OBool (n1 == n2))
                NEqual -> return (OBool (n1 /= n2))
            _ -> raiseTypeError ("You are comparing " ++
                "number to non number value!")
                (printTree (ERel e1 op e2))
        (OString s1) -> case v2 of
            (OString s2) -> case op of
                Less -> return (OBool (s1 < s2))
                LessOrEq -> return (OBool (s1 <= s2))
                Greater -> return (OBool (s1 > s2))
                GrtOrEq -> return (OBool (s1 >= s2))
                Equal -> return (OBool (s1 == s2))
                NEqual -> return (OBool (s1 /= s2))
            _ -> raiseTypeError ("You are comparing " ++
                "string value to non string value!")
                (printTree (ERel e1 op e2))
        (OBool b1) -> case op of
            Equal -> case v2 of
                (OBool b2) -> return (OBool (b1 == b2))
                _ -> raiseTypeError ("You are comparing" ++
                    "boolean to non boolean value")
                    (printTree (ERel e1 op e2))
            NEqual -> case v2 of
                (OBool b2) -> return (OBool (b1 /= b2))
                _ -> raiseTypeError ("You are comparing" ++
                    "boolean to non boolean value")
                    (printTree (ERel e1 op e2))
            _ -> raiseTypeError ("You are comparing " ++
                "objects that are not comparable!")
                (printTree (ERel e1 op e2))
        (OList t1 xs) -> case v2 of
            (OList t2 ys) -> case op of
                Equal -> undefined
                NEqual -> undefined
                _ -> undefined
            _ -> undefined
        (ODict kt1 vt1 m1) -> case v2 of
            (ODict kt2 vt2 m2) -> case op of
                Equal -> undefined
                NEqual -> undefined
                _ -> undefined
            _ -> undefined
        (NULL) -> undefined

eval (EAnd e1 e2) = do
    v1 <- myCatchError (eval e1)
        (\er -> myThrowError er)
    v2 <- myCatchError (eval e2)
        (\er -> myThrowError er)
    case v1 of
        (OBool b1) -> case v2 of
            (OBool b2) -> return (OBool (b1 && b2))
            _ -> raiseTypeError ("You are trying and statement for variable" ++ 
                " that is not a boolean!") (printTree (EAnd e1 e2))
        _ -> raiseTypeError ("You are trying and statement for variable" ++ 
            " that is not a boolean!") (printTree (EAnd e1 e2))


eval (EOr e1 e2) = do
    v1 <- myCatchError (eval e1)
        (\er -> myThrowError er)
    v2 <- myCatchError (eval e2)
        (\er -> myThrowError er)
    case v1 of
        (OBool b1) -> case v2 of
            (OBool b2) -> return (OBool (b1 || b2))
            _ -> raiseTypeError ("You are trying or statement for variable" ++ 
                " that is not a boolean!") (printTree (EOr e1 e2))
        _ -> raiseTypeError ("You are trying or statement for variable" ++ 
            " that is not a boolean!") (printTree (EOr e1 e2))


-- Fully evaluating just an expressiong - pretty meaningless for interpreter
-- Not used!
evalExp :: Exp -> IO Object
evalExp e = do
    val <- (evalStateT $ runErrorT (runReaderT (eval e) 
        (Env { variables = M.empty
             , functions = M.empty }))) 
        (Store { objects = M.empty
               , funs = M.empty})
    case val of
        (Right x) -> return x
        (Left x) -> error ("Encountered error: " ++ show(x))


-- Function for executing statements.
execStmt :: Stmt -> RES Object

-- Block statement
execStmt (BStmt b) = case b of
    (SBlock []) -> return NULL
    (SBlock (s:ss)) -> do
        execStmt s
        execStmt (BStmt (SBlock ss))
    (DBlock [] ss) -> do
        execStmt (BStmt (SBlock (ss)))
    (DBlock (d:dd) (s:ss)) -> do
        case d of
            (TDecl t x) -> do
                store <- myGet
                objs <- myGets objects
                let location = alloc objs
                myPut (updateStateReference x location t store)                        
                local (updateVariable x location) 
                    (execStmt (BStmt (DBlock (dd) (s:ss))))
            (AssDecl t x e) -> do
                val <- myCatchError (eval e) 
                    (\er -> myThrowError (replaceStmt er (printTree d)))
                if (isTypeOf t val) then do
                    objs <- myGets objects
                    store <- myGet
                    let location = alloc objs
                    myPut 
                        (updateStateReferenceWithValue x location val store)
                    local (updateVariable x location) 
                        (execStmt (BStmt (DBlock (dd) (s:ss))))
                else
                    raiseTypeError 
                        ("You are trying to assign object of different type!")
                        (printTree (d))
            (FunDecl t x a b) -> do
                store <- myGet
                fs <- myGets funs
                let location = alloc fs
                myPut (updateStateFunctions t x a b location store)
                local (updateFunctions x location) 
                    (execStmt (BStmt (DBlock (dd) (s:ss))))

-- If statement
execStmt (Cond e s) = do
    v <- myCatchError (eval e)
        (\er -> myThrowError (replaceStmt er (printTree (Cond e s))))
    case v of
        (OBool True) -> execStmt s
        (OBool False) -> return NULL
        _ -> raiseTypeError ("You are passing something other than" ++ 
            " boolean value to if!") (printTree (Cond e s))


-- If else statement
execStmt (CondElse e s1 s2) = do
    v <- myCatchError (eval e)
        (\er -> myThrowError (replaceStmt er (printTree (CondElse e s1 s2))))
    case v of
        (OBool True) -> execStmt s1
        (OBool False) -> execStmt s2
        _ -> raiseTypeError ("You are passing something other than" ++ 
            " boolean value to if!") (printTree (CondElse e s1 s2))

-- While statement
execStmt (While e s) = do
    v <- myCatchError (eval e) 
        (\er -> myThrowError (replaceStmt er (printTree (While e s))))
    case v of
        (OBool True) -> do
            execStmt s 
            execStmt (While e s)
        (OBool False) -> return NULL
        _ -> raiseTypeError ("You are passing something other than" ++ 
            " boolean value to while!") (printTree (While e s))


-- For statement
execStmt (For x e1 e2 s) = do
    objs <- myGets objects
    vars <- asks variables
    store <- myGet
    case M.lookup x vars of
        (Just loc) -> case M.lookup loc objs of
            (Just obj) -> case obj of
                (OInt _) -> do
                    v1 <- myCatchError (eval e1) 
                        (\er -> myThrowError (replaceStmt er 
                            (printTree (For x e1 e2 s))))
                    v2 <- myCatchError (eval e2) 
                        (\er -> myThrowError (replaceStmt er 
                            (printTree (For x e1 e2 s))))
                    case v1 of
                        (OInt n1) -> case v2 of
                            (OInt n2) -> if (n1 <= n2)
                                then do
                                    myPut (
                                        updateStateReferenceWithValue x 
                                            loc v1 store)
                                    execStmt (s)
                                    newObjs <- myGets objects
                                    -- Checking if value was not change
                                    -- Guarding the variable
                                    case M.lookup loc newObjs of
                                        (Just newObj) -> case newObj of
                                            (OInt value) -> do
                                                if (n1 /= value) then do
                                                    raiseForVariableChanged x
                                                        (printTree 
                                                            (For x e1 e2 s))
                                                else do
                                                    execStmt (For x 
                                                        (EInt (n1 + 1)) 
                                                        (EInt (n2)) s)
                                            _ -> raiseForVariableChanged x
                                                (printTree (For x e1 e2 s))
                                        (Nothing) -> raiseNullException x
                                            (printTree (For x e1 e2 s))
                                else
                                    return NULL
                            _ -> raiseTypeError ("You are trying to assign "
                                ++ "expression of different type" ++
                                "than the variable!") 
                                (printTree (For x e1 e2 s))
                        _ -> raiseTypeError ("You are trying to assign" ++
                            "expression of different type than the variable!")
                            (printTree (For x e1 e2 s)) 
                _ -> raiseTypeError ("You are trying to assign expression "
                    ++ "of different type than the variable!") 
                    (printTree (For x e1 e2 s))
            (Nothing) -> raiseNullException x (printTree (For x e1 e2 s))
        (Nothing) -> raiseUndeclaredVariable x (printTree (For x e1 e2 s))

-- Return statement
execStmt (Ret e) = do
    val <- myCatchError (eval e) 
        (\er -> myThrowError (replaceStmt er (printTree (Ret e))))
    returnObject val
    return val

-- Print statement
execStmt (Print e) = do
    v <- myCatchError (eval e) 
        (\er -> myThrowError (replaceStmt er (printTree (Print e))))
    liftToIO $ putStr $ show v
    return NULL

-- Assignment statement
execStmt (Assign x e) = do
    vars <- asks variables
    case M.lookup x vars of
        (Just l) -> do
            objs <- myGets objects
            val <- myCatchError (eval e)
                (\er -> myThrowError (replaceStmt er (printTree (Assign x e))))
            case M.lookup l objs of
                (Just obj) -> if (equalTypes obj val)
                    then do
                        store <- myGet
                        myPut 
                            (updateStateReferenceWithValue x l val store)
                        return NULL
                    else
                        raiseTypeError ("You are trying to assign expression "
                            ++ "of different type than the variable!")
                            (printTree (Assign x e))
                (Nothing) -> raiseNullException x (printTree (Assign x e))
        (Nothing) -> raiseUndeclaredVariable x (printTree (Assign x e))

-- Increase statement
execStmt (AssignInc x e) = do
    vars <- asks variables
    case M.lookup x vars of
        (Just loc) -> do
            objs <- myGets objects
            val <- myCatchError (eval e)
                (\er -> myThrowError (replaceStmt er 
                    (printTree (AssignInc x e))))
            case M.lookup loc objs of
                (Just obj) -> case obj of
                    (OInt n1) -> case val of
                        (OInt n2) -> do
                            store <- myGet
                            myPut
                                (updateStateReferenceWithValue x loc
                                    (OInt (n1 + n2)) store)
                            return NULL
                        _ -> raiseTypeError ("You are trying to add number "
                            ++ " to something that is not number") 
                            (printTree (AssignInc x e))
                    (OString s1) -> case val of
                        (OInt n2) -> do
                            store <- myGet
                            myPut 
                                (updateStateReferenceWithValue x loc 
                                    (OString (s1 ++ (show n2))) store)
                            return NULL
                        (OString s2) -> do
                            store <- myGet
                            myPut 
                                (updateStateReferenceWithValue x loc 
                                    (OString (s1 ++ s2)) store)
                            return NULL
                    _ -> raiseTypeError ("You are trying an increase operation "
                        ++ "on a variable that has no such operation!") 
                        (printTree (AssignInc x e))
                (Nothing) -> raiseNullException x (printTree (AssignInc x e))
        (Nothing) -> raiseUndeclaredVariable x (printTree (AssignInc x e))

-- Decrease statement
execStmt (AssignDec x e) = do
    vars <- asks variables
    case M.lookup x vars of
        (Just loc) -> do
            objs <- myGets objects
            val <- myCatchError (eval e)
                (\er -> myThrowError (replaceStmt er 
                    (printTree (AssignDec x e))))
            case M.lookup loc objs of
                (Just obj) -> case obj of
                    (OInt n1) -> case val of
                        (OInt n2) -> do
                            store <- myGet
                            myPut
                                (updateStateReferenceWithValue x loc
                                    (OInt (n1 - n2)) store)
                            return NULL
                        _ -> raiseTypeError ("You are trying to subtract number"
                            ++ " to something that is not number") 
                            (printTree (AssignDec x e))
                    _ -> raiseTypeError ("You are trying a decrease operation "
                        ++ "on a variable that has no such operation!")
                        (printTree (AssignDec x e)) 
                (Nothing) -> raiseNullException x (printTree (AssignDec x e))
        (Nothing) -> raiseUndeclaredVariable x (printTree (AssignDec x e))

-- Increase by 1 (++)
execStmt (Incr x) = execStmt (AssignInc x (EInt 1))

-- Decrease by 1 (--)
execStmt (Decr x) = execStmt (AssignDec x (EInt 1))

-- List/Dict functions are not defined yet
execStmt (Add x e) = undefined

execStmt (Remove x e) = undefined

execStmt (Insert x e1 e2) = undefined    

-- Throwing exceptions
execStmt (Except e) = do
    val <- myCatchError (eval e)
        (\er -> myThrowError (replaceStmt er (printTree (Except e))))
    myThrowError (Exception val (printTree (Except e)))
    return NULL

-- Try catch
execStmt (TryCatch b1 x b2) = do
    myCatchError (execStmt (BStmt b1))
        (\er -> case er of
            (Exception e _) -> do
                objs <- myGets objects
                store <- myGet
                let location = alloc objs
                myPut (
                    updateStateReferenceWithValue x location e store)
                local (updateVariable x location) 
                    (execStmt (BStmt b2))
            (ZeroDivision _) -> do
                objs <- myGets objects
                store <- myGet
                let location = alloc objs
                myPut (
                    updateStateReferenceWithValue x location 
                        (OString ("ZeroDivisionException")) store)
                local (updateVariable x location)
                    (execStmt (BStmt b2)) 
            _ -> error (show er)
        )
    return NULL

-- Just expression statement.
execStmt (SExp e) = do
    val <- myCatchError (eval e)
        (\er -> myThrowError (replaceStmt er (printTree (SExp e))))
    return val

-- Empty statement
execStmt (Empty) = return NULL

-- Ensuring only Program can be executed
class Executable a where
    exec :: a -> IO ()

instance Executable Program where
    exec (Main block) = do
        (res, state) <- (runStateT $ runErrorT 
            (runReaderT (execStmt (BStmt block))
                (Env { variables = M.empty 
                     , functions = M.empty }))) 
                (Store { objects = M.empty
                       , funs = M.empty })
        case res of
            (Right _) -> putStrLn ""
            (Left e) -> case e of 
                (Return _) -> putStrLn ""
                _ -> error $ show e

execProgram :: Executable a => a -> IO ()
execProgram = exec
