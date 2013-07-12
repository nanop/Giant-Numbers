module HBin where
import Visuals

data T = E | V T [T]  | W T [T] deriving (Eq,Show,Read)

n E = 0
n (V x []) = 2^(n x +1)-1
n (V x (y:xs)) = (n u+1)*2^(n x + 1)-1 where u = W y xs
n (W x []) = 2^(n x+2)-2
n (W x (y:xs)) = (n u+2)*2^(n x + 1)-2 where u = V y xs  

s E = V E []
s (V E []) =  W E []
s (V E (x:xs)) =  W (s x) xs  
s (V z xs) = W E (s' z : xs)
s (W z [])  = V (s z) []
s (W z [E]) = V z [E]
s (W z (E:y:ys)) = V z (s y:ys)
s (W z (x:xs)) = V z (E:s' x:xs)

s' (V E []) = E
s' (V z []) = W (s' z) []
s' (V z [E]) =  W z [E]
s' (V z (E:x:xs)) =  W z (s x:xs)  
s' (V z (x:xs)) =  W z (E:s' x:xs)  
s' (W E []) = V E []
s' (W E (x:xs)) = V (s x) xs 
s' (W z xs) = V E (s' z:xs)

o E = V E []
o (V x xs) = V (s x) xs
o (W x xs) = V E (x:xs)

i E = W E []
i (V x xs) = W E (x:xs)
i (W x xs) = W (s x) xs

o' (V E []) = E
o' (V E (x:xs)) = W x xs
o' (V x xs) = V (s' x) xs

i' (W E []) = E
i' (W E (x:xs)) = V x xs
i' (W x xs) = W (s' x) xs

o_ (V _ _ ) = True
o_ _ = False

i_ (W _ _ ) = True
i_ _ = False

t 0 = E
t x | x>0 && odd x = o(t (div (pred x) 2))
t x | x>0 && even x = i(t (pred (div x 2)))

db = s' . o
hf = o' . s 

exp2 E = V E []
exp2 x = s (V (s' x) [])

simpleAdd E y = y
simpleAdd x E = x
simpleAdd x y | o_ x && o_ y = 
  i (simpleAdd (o' x) (o' y))
simpleAdd x y | o_ x && i_ y = 
  o (s (simpleAdd (o' x) (i' y)))
simpleAdd x y | i_ x && o_ y = 
  o (s (simpleAdd (i' x) (o' y)))
simpleAdd x y | i_ x && i_ y = 
  i (s (simpleAdd (i' x) (i' y)))

simpleSub x E= x
simpleSub y x | o_ y && o_ x = 
  s' (o (simpleSub (o' y) (o' x))) 
simpleSub y x | o_ y && i_ x = 
  s' (s' (o (simpleSub (o' y) (i' x))))
simpleSub y x | i_ y && o_ x = 
  o (simpleSub (i' y) (o' x))  
simpleSub y x | i_ y && i_ x = 
  s' (o (simpleSub (i' y) (i' x))) 

otimes E y = y
otimes n E = V (s' n) []
otimes n (V y ys) = V (add n y) ys
otimes n (W y ys) = V (s' n) (y:ys)

itimes E y = y
itimes n E = W (s' n) []
itimes n (W y ys) = W (add n y) ys
itimes n (V y ys) = W (s' n) (y:ys)    

oplus k x y =  itimes k (add x y) 
   
oiplus k x y = s' (itimes k (s (add x y)))    
   
iplus k x y = s' (s' (itimes k (s (s (add x y)))))

ominus _ x y | x == y = E
ominus k x y = s (otimes k (s' (sub x y))) 

iminus _ x y | x == y = E   
iminus k x y =  s (otimes k (s' (sub x y)))

oiminus k x y | x==s y = s E
oiminus k x y | x == s (s y) = s (exp2 k)
oiminus k x y =  s (s (otimes k (s' (s' (sub x y)))))   

iominus k x y = otimes k (sub x y)

osplit (V x []) = (x,E )
osplit (V x (y:xs)) = (x,W y xs)

isplit (W x []) = (x,E )
isplit (W x (y:xs)) = (x,V y xs)

add E y = y
add x E = x

add x y |o_ x && o_ y = f (cmp a b) where
  (a,as) = osplit x
  (b,bs) = osplit y
  f EQ = oplus (s a) as bs
  f GT = oplus (s b) (otimes (sub a b) as) bs
  f LT = oplus (s a) as (otimes (sub b a) bs)

add x y |o_ x && i_ y = f (cmp a b) where
  (a,as) = osplit x
  (b,bs) = isplit y
  f EQ = oiplus (s a) as bs
  f GT = oiplus (s b) (otimes (sub a b) as) bs
  f LT = oiplus (s a) as (itimes (sub b a) bs)  

add x y |i_ x && o_ y = f (cmp a b) where
  (a,as) = isplit x
  (b,bs) = osplit y
  f EQ = oiplus (s a) as bs
  f GT = oiplus (s b) (itimes (sub a b) as) bs
  f LT = oiplus (s a) as (otimes (sub b a) bs)    

add x y |i_ x && i_ y = f (cmp a b) where
  (a,as) = isplit x
  (b,bs) = isplit y
  f EQ = iplus (s a) as bs
  f GT = iplus (s b) (itimes (sub a b) as) bs
  f LT = iplus (s a) as (itimes (sub b a) bs)  

sub x E = x
sub x y | o_ x && o_ y = f (cmp a b) where
  (a,as) = osplit x
  (b,bs) = osplit y
  f EQ = ominus (s a) as bs
  f GT = ominus (s b) (otimes (sub a b) as) bs
  f LT = ominus (s a) as (otimes (sub b a) bs)

sub x y |o_ x && i_ y = f (cmp a b) where
  (a,as) = osplit x
  (b,bs) = isplit y
  f EQ = oiminus (s a) as bs
  f GT = oiminus (s b) (otimes (sub a b) as) bs
  f LT = oiminus (s a) as (itimes (sub b a) bs)  
sub x y |i_ x && o_ y = f (cmp a b) where
  (a,as) = isplit x
  (b,bs) = osplit y
  f EQ = iominus (s a) as bs
  f GT = iominus (s b) (itimes (sub a b) as) bs
  f _ = iominus (s a) as (otimes (sub b a) bs)      
sub x y |i_ x && i_ y = f (cmp a b) where
  (a,as) = isplit x
  (b,bs) = isplit y
  f EQ = iminus (s a) as bs
  f GT = iminus (s b) (itimes (sub a b) as) bs
  f LT = iminus (s a) as (itimes (sub b a) bs)  

cmp E E = EQ
cmp E _ = LT
cmp _ E = GT
cmp x y | x' /= y'  = cmp x' y' where
  x' = bitsize x
  y' = bitsize y
cmp x y = 
  compBigFirst (reversedDual x) (reversedDual y)

compBigFirst E E = EQ
compBigFirst x y | o_ x && o_ y = f (cmp a b) where
    (a,c) = osplit x
    (b,d) = osplit y
    f EQ = compBigFirst c d
    f LT = GT
    f GT = LT   
compBigFirst x y | i_ x && i_ y = f (cmp a b) where
    (a,c) = isplit x
    (b,d) = isplit y
    f EQ = compBigFirst c d
    f other = other
compBigFirst x y | o_ x && i_ y = LT
compBigFirst x y | i_ x && o_ y = GT

reversedDual E = E
reversedDual (V x xs) = f (len (y:ys)) where
  (y:ys) = reverse (x:xs)
  f l | o_ l = V y ys
  f l | i_ l = W y ys
reversedDual (W x xs) = f (len (y:ys)) where
  (y:ys) = reverse (x:xs)
  f l | o_ l = W y ys
  f l | i_ l = V y ys

len [] = E
len (_:xs) = s (len xs)  

min2 x y = if LT==cmp x y then x else y
max2 x y = if LT==cmp x y then y else x

dual E = E
dual (V x xs) = W x xs
dual (W x xs) = V x xs

bitsize E = E
bitsize (V x xs) = s (foldr add1 x xs)
bitsize (W x xs) = s (foldr add1 x xs)

add1 x y = s (add x y)

ilog2 x = bitsize (s' x)

leftshiftBy _ E = E
leftshiftBy n k = s (otimes n (s' k))

toShift x | o_ x = (o E,m,k) where
  (a,b) = osplit x
  m = s a
  k = s b
toShift x | i_ x = (i E,m,k) where
  (a,b) = isplit x
  m = s a
  k = s (s b)

rightshiftBy _ E = E
rightshiftBy m k = f (cmp m a) where
  (p,a,b) = toShift k
  
  f EQ | o_ p = sub b p
  f EQ | i_ p = s (sub b p)
  f LT | o_ p = otimes (sub a m) (sub b p)
  f LT | i_ p = s (itimes (sub a m) (sub b p))
  f GT =  rightshiftBy (sub m a) b

mul _ E = E
mul E _ = E

mul x  y | o_ x && o_ y = r2 where
  (n,a) = osplit x
  (m,b) = osplit y
  p1 = add (mul a b) (add a b)
  p2 = otimes (add (s n) (s m)) p1
  r1 = sub p2 x
  r2 = sub r1 y  

mul x y | o_ x && i_ y = add x (mul x (s' y))
mul x y | i_ x && o_ y = add y (mul (s' x) y)
mul x y | i_ x && i_ y = 
  s (add (add x' y') (mul x' y')) where 
    x'=s' x 
    y'=s' y

square E = E
square x | o_ x = r where
  (n,a) = osplit x
  p1 = add (square a) (db a)
  p2 = otimes (i n) p1
  r = sub p2 (db x) 
square x| i_ x = s (add (db x') (square x')) where 
  x' = s' x


pow _ E = V E []
pow x y | o_ y = mul x (pow (square x) (o' y))
pow x y | i_ y = mul x2 (pow x2 (i' y)) where 
  x2 = square x

tsize E = E
tsize (V x xs) = foldr add1 E (map tsize (x:xs))
tsize (W x xs) = foldr add1 E (map tsize (x:xs))

repcomp m =  [bsizes,tsizes] where
  ts = map t [0..2^m-1]
  bsizes = map (n.bitsize) ts
  tsizes = map (n.tsize) ts
  
repdif x = n (sub (bitsize (t x)) (tsize (t x)))  

iterated f E x = x
iterated f k x = f (iterated f (s' k) x) 

bestCase k = s' (iterated exp2 k E)

worseCase k = iterated (i.o) k E 

fermat n = s (exp2 (exp2 n))

mersenne p = s' (exp2 p)

perfect p = s (V q [q]) where q = s' (s' p)

-- exponent of the 48-th known Mersenne prime
prime48 = 57885161
-- the actual Mersenne prime
mersenne48 = s' (exp2 (t prime48))

perfect48 = perfect (t prime48)

genFermatPrime = s (leftshiftBy n k) where 
  n = t (9167433::Integer)
  k = t (27653::Integer)

cullenPrime = s (leftshiftBy n n) where 
  n = t (6679881::Integer)

woodallPrime = s' (leftshiftBy n n) where 
  n = t (3752948::Integer)

prothPrime = s (leftshiftBy n k) where 
  n = t (13018586::Integer)
  k = t (19249::Integer)

sophieGermainPrime = s' (leftshiftBy n k) where 
  n = t (666667::Integer)
  k = t (18543637900515::Integer)

twinPrimes = (s' m,s m) where 
  n = t (666669::Integer)
  k = t (3756801695685::Integer)
  m = leftshiftBy n k

tl n | o_ n = o' n 
tl n | i_ n = f xs where 
  V _ xs = s' n
  f [] = E
  f (y:ys) = s (i' (W y ys)) 

cons n y = s (otimes n (s' (o y)))

hd z | o_ z = E
hd z = s x where V x _ = s' z

syracuse n = tl (add n (i n))

nsyr E = [E]
nsyr n = n : nsyr (syracuse n)

div_and_rem x y | LT == cmp x y = (E,x)
div_and_rem x y | y /= E = (q,r) where 
  (qt,rm) = divstep x y
  (z,r) = div_and_rem rm y
  q = add (exp2 qt) z
    
  divstep n m = (q, sub n p) where
    q = try_to_double n m E
    p = leftshiftBy q m    
    
  try_to_double x y k = 
    if (LT==cmp x y) then s' k
    else try_to_double x (db y) (s k)   
          
divide n m = fst (div_and_rem n m)
remainder n m = snd (div_and_rem n m)

isqrt E = E
isqrt n = if cmp (square k) n==GT then s' k else k where 
  two = i E
  k=iter n
  iter x = if cmp (absdif r x)  two == LT
    then r 
    else iter r where r = step x
  step x = divide (add x (divide n x)) two    
  
absdif x y = if LT == cmp x y then sub y x else sub x y  

-- experiments
 
collatz E = E
collatz x| o_ x = add x (o x)
collatz x| i_ x = hf x

ncollatz E = [E]
ncollatz x = x : ncollatz (collatz x)

c0 = take 1000  $(map (n.tsize) (ncollatz mersenne48))
c1 = take 100000  $(map (n.tsize) (ncollatz perfect48))
c2 = take 1000  $(map (n.tsize) (ncollatz prothPrime))
c3 = take 1000  $(map (n.tsize) (ncollatz cullenPrime))
c4 = take 1000  $(map (n.tsize) (ncollatz genFermatPrime))
c5 = take 1000  $(map (n.tsize) (ncollatz woodallPrime))
c6 = take 1000  $(map (n.tsize) (ncollatz sophieGermainPrime))

-- the syracuse sequence on large primes
t0 = take 1000  $(map (n.tsize) (nsyr mersenne48))
t1 = take 1000  $(map (n.tsize) (nsyr perfect48))
t2 = take 1000  $(map (n.tsize) (nsyr prothPrime))
t3 = take 1000  $(map (n.tsize) (nsyr cullenPrime))
t4 = take 1000  $(map (n.tsize) (nsyr genFermatPrime))
t5 = take 1000  $(map (n.tsize) (nsyr woodallPrime))
t6 = take 1000  $(map (n.tsize) (nsyr sophieGermainPrime))

tc = cmp (add cullenPrime  mersenne48) (add prothPrime mersenne48)

tf x = map (n.tsize) (nsyr (fermat (t x)))
tm x = map (n.tsize) (nsyr (mersenne (t x)))

assertOplus k x y =  (a==b) where
   a = add (otimes k x) (otimes k y)
   b = itimes k (add x y) 
   
assertIplus k x y =  (a==b) where
   a = add (itimes k x) (itimes k y)
   b = s' (s' (itimes k (s (s (add x y)))))
   
assertOiplus k x y =  (a==b) where
   a = add (otimes k x) (itimes k y) 
   b = s' (itimes k (s (add x y)))   

assertIoplus k x y =  (a==b) where
   a = add (itimes k x) (otimes k y) 
   b = s' (itimes k (s (add x y)))   

