-- Nat
data Nat = Zero | Succ Nat
    deriving Show

-- nat2int
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

-- int2nat
int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

-- add
add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)

-- (mult)
mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult m (Succ n) = add m (mult m n)
