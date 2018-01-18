module January11() where

newtype Plus = Plus Int deriving (Show)

instance Monoid Plus where
    mempty = Plus 1
    (Plus x) `mappend` (Plus y) = Plus (x + y)


newtype Produkt = Produkt Int deriving (Show)

instance Monoid Produkt where
    mempty = Produkt 1
    (Produkt x) `mappend` (Produkt y) = Produkt (x * y)


data Mebbe a = Jus a | Nothng deriving (Show)

instance Monoid a => Monoid (Mebbe a) where
    mempty = Nothng
    mappend (Jus a) (Jus b) = Jus (mappend a b)
    mappend (Jus a) _ = Jus a
    mappend _ (Jus a) = Jus a
    mappend Nothng Nothng = Nothng
