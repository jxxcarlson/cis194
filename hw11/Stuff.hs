

-- instance Functor ((->) e) where
--   fmap = (.)
--
-- instance Applicative ((->) e) where
--   pure = const
--   f <*> x = \e -> (f e) (x e)


liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 h fa fb = h <$> fa <*> fb

-- liftCons = liftA2 (:)

type Name = String

data Employee = Employee {name :: Name, phone :: String} deriving Show

data BigRecord = BR { getName         :: Name
                    , getSSN          :: String
                    , getSalary       :: Integer
                    , getPhone        :: String
                    , getLicensePlate :: String
                    , getNumSickDays  :: Int
                    } deriving Show

r = BR "Brent" "XXX-XX-XXX4" 600000000 "555-1234" "JGX-55T3" 2


getEmp :: BigRecord -> Employee
getEmp = Employee <$> getName <*> getPhone

ex01 = getEmp r


--- IO ---


main = do line <- getLine
          let line' = reverse line
          putStrLn $ "Yes, you really said (" ++ line' ++ ") backwards!"

-- main' = do line <- (fmap reverse getLine)
--           putStrLn $ "Yes, you really said (" ++ line ++ ") backwards!"

-- main' = do line <- fmap reverse getLine
--           putStrLn $ "You said " ++ line ++ " backwards!"
--           putStrLn $ "Yes, you really said" ++ line ++ " backwards!"


-- instance Functor ((->) r) where
--     fmap f g = (\x -> f (g x))
