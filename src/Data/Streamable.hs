import qualified Data.Vector.Fusion.Stream as Stream
import qualified Data.Vector.Fusion.Stream.Monadic as MStream

import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S

{-# RULES
  "stream/unstream" forall s . stream (unstream s) = s
  #-}

class Streamable seq where
    type Element seq
    stream :: Monad m => seq -> MStream.Stream m (Element seq)
    unstreamM :: Monad m => MStream.Stream m (Element seq) -> seq

unstream :: Streamable seq => Stream.Stream (Element seq) -> seq
unstream = unId . unstreamM

instance Streamable [a] where
    type Element [a] = a
    stream = MStream.fromList
    unstreamM = MStream.toList

instance Streamable (P.Vector a) where
    type Element (P.Vector a) = a
    stream = P.stream
    unstreamM = P.unstreamM

instance Streamable (U.Vector a) where
    type Element (U.Vector a) = a
    stream = U.stream
    unstreamM = U.unstreamM

instance Streamable (S.Vector a) where
    type Element (S.Vector a) = a
    stream = S.stream
    unstreamM = S.unstreamM

-- Initialisation
-- --------------

-- | /O(1)/ Empty vector
empty :: Streamable v => v
{-# INLINE empty #-}
empty = unstream Stream.empty

-- | /O(1)/ Vector with exactly one element
singleton :: Streamable v => Element v -> v
{-# INLINE singleton #-}
singleton x = unstream (Stream.singleton x)

-- | /O(n)/ Vector of the given length with the same value in each position
replicate :: Streamable v => Int -> Element v -> v
{-# INLINE replicate #-}
replicate n x = unstream (Stream.replicate n x)

-- | /O(n)/ Construct a vector of the given length by applying the function to
-- each index
generate :: Streamable v => Int -> (Int -> Element v) -> v
{-# INLINE generate #-}
generate n f = unstream (Stream.generate n f)

-- | /O(n)/ Apply function n times to value. Zeroth element is original value.
iterateN :: Streamable v => Int -> (Element v -> Element v) -> Element v -> v
{-# INLINE iterateN #-}
iterateN n f x = unstream (Stream.iterateN n f x)

-- Unfolding
-- ---------

-- | /O(n)/ Construct a vector by repeatedly applying the generator function
-- to a seed. The generator function yields 'Just' the next element and the
-- new seed or 'Nothing' if there are no more elements.
--
-- > unfoldr (\n -> if n == 0 then Nothing else Just (n,n-1)) 10
-- >  = <10,9,8,7,6,5,4,3,2,1>
unfoldr :: Streamable v => (b -> Maybe (Element v, b)) -> b -> v
{-# INLINE unfoldr #-}
unfoldr f = unstream . Stream.unfoldr f

-- | /O(n)/ Construct a vector with at most @n@ by repeatedly applying the
-- generator function to the a seed. The generator function yields 'Just' the
-- next element and the new seed or 'Nothing' if there are no more elements.
--
-- > unfoldrN 3 (\n -> Just (n,n-1)) 10 = <10,9,8>
unfoldrN  :: Streamable v => Int -> (b -> Maybe (Element v, b)) -> b -> v
{-# INLINE unfoldrN #-}
unfoldrN n f = unstream . Stream.unfoldrN n f

-- Enumeration
-- -----------

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+1@
-- etc. This operation is usually more efficient than 'enumFromTo'.
--
-- > enumFromN 5 3 = <5,6,7>
enumFromN :: (Streamable v, Num (Element v)) => Element v -> Int -> v
{-# INLINE enumFromN #-}
enumFromN x n = enumFromStepN x 1 n

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than 'enumFromThenTo'.
--
-- > enumFromStepN 1 0.1 5 = <1,1.1,1.2,1.3,1.4>
enumFromStepN :: (Streamable v, Num (Element v)) => Element v -> Element v -> Int -> v
{-# INLINE enumFromStepN #-}
enumFromStepN x y n = unstream
                    $ Stream.enumFromStepN  x y n

-- | /O(n)/ Enumerate values from @x@ to @y@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromN' instead.
enumFromTo :: (Streamable v, Enum (Element v)) => Element v -> Element v -> v
{-# INLINE enumFromTo #-}
enumFromTo x y = unstream (Stream.enumFromTo x y)

-- | /O(n)/ Enumerate values from @x@ to @y@ with a specific step @z@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: (Streamable v, Enum (Element v)) => Element v -> Element v -> Element v -> v
{-# INLINE enumFromThenTo #-}
enumFromThenTo x y z = unstream (Stream.enumFromThenTo x y z)

-- Concatenation
-- -------------

-- | /O(n)/ Prepend an element
cons :: Streamable v => Element v -> v -> v
{-# INLINE cons #-}
cons x v = unstream
         $ Stream.cons x
         $ stream v

-- | /O(n)/ Append an element
snoc :: Streamable v => v -> Element v -> v
{-# INLINE snoc #-}
snoc v x = unstream
         $ Stream.snoc (stream v) x

infixr 5 ++
-- | /O(m+n)/ Concatenate two vectors
(++) :: Streamable v => v -> v -> v
{-# INLINE (++) #-}
v ++ w = unstream (stream v Stream.++ stream w)

{-
-- | /O(n)/ Concatenate all vectors in the list
concat :: Vector v a => [v a] -> v a
{-# INLINE concat #-}
concat vs = unstream (Stream.flatten mk step (Exact n) (Stream.fromList vs))
  where
    n = List.foldl' (\k v -> k + length v) 0 vs

    {-# INLINE_INNER step #-}
    step (v,i,k)
      | i < k = case unsafeIndexM v i of
                  Box x -> Stream.Yield x (v,i+1,k)
      | otherwise = Stream.Done

    {-# INLINE mk #-}
    mk v = let k = length v
           in
           k `seq` (v,0,k)

-- Monadic initialisation
-- ----------------------

-- | /O(n)/ Execute the monadic action the given number of times and store the
-- results in a vector.
replicateM :: (Monad m, Vector v a) => Int -> m a -> m (v a)
{-# INLINE replicateM #-}
replicateM n m = unstreamM (MStream.replicateM n m)

-- | /O(n)/ Construct a vector of the given length by applying the monadic
-- action to each index
generateM :: (Monad m, Vector v a) => Int -> (Int -> m a) -> m (v a)
{-# INLINE generateM #-}
generateM n f = unstreamM (MStream.generateM n f)

-- Permutations
-- ------------

-- | /O(n)/ Reverse a vector
reverse :: (Vector v a) => v a -> v a
{-# INLINE reverse #-}
-- FIXME: make this fuse better, add support for recycling
reverse = unstream . streamR

-- | /O(n)/ Yield the vector obtained by replacing each element @i@ of the
-- index vector by @xs'!'i@. This is equivalent to @'map' (xs'!') is@ but is
-- often much more efficient.
--
-- > backpermute <a,b,c,d> <0,3,2,3,1,0> = <a,d,c,d,b,a>
backpermute :: (Vector v a, Vector v Int)
            => v a   -- ^ @xs@ value vector
            -> v Int -- ^ @is@ index vector (of length @n@)
            -> v a
{-# INLINE backpermute #-}
-- This somewhat non-intuitive definition ensures that the resulting vector
-- does not retain references to the original one even if it is lazy in its
-- elements. This would not be the case if we simply used map (v!)
backpermute v is = seq v
                 $ seq n
                 $ unstream
                 $ Stream.unbox
                 $ Stream.map index
                 $ stream is
  where
    n = length v

    {-# INLINE index #-}
    -- NOTE: we do it this way to avoid triggering LiberateCase on n in
    -- polymorphic code
    index i = BOUNDS_CHECK(checkIndex) "backpermute" i n
            $ basicUnsafeIndexM v i

-- | Same as 'backpermute' but without bounds checking.
unsafeBackpermute :: (Vector v a, Vector v Int) => v a -> v Int -> v a
{-# INLINE unsafeBackpermute #-}
unsafeBackpermute v is = seq v
                       $ seq n
                       $ unstream
                       $ Stream.unbox
                       $ Stream.map index
                       $ stream is
  where
    n = length v

    {-# INLINE index #-}
    -- NOTE: we do it this way to avoid triggering LiberateCase on n in
    -- polymorphic code
    index i = UNSAFE_CHECK(checkIndex) "unsafeBackpermute" i n
            $ basicUnsafeIndexM v i

-}

-- Indexing
-- --------

-- | /O(n)/ Pair each element in a vector with its index
indexed :: (Streamable v, Streamable v', Element v' ~ (Int, Element v)) => v -> v'
{-# INLINE indexed #-}
indexed = unstream . Stream.indexed . stream

-- Mapping
-- -------

-- | /O(n)/ Map a function over a vector
map :: (Streamable v, Streamable v') => (Element v -> Element v') -> v -> v'
{-# INLINE map #-}
map f = unstream . inplace (MStream.map f) . stream

-- | /O(n)/ Apply a function to every element of a vector and its index
imap :: (Streamable v, Streamable v') => (Int -> Element v -> Element v') -> v -> v'
{-# INLINE imap #-}
imap f = unstream . inplace (MStream.map (uncurry f) . MStream.indexed)
                  . stream

{-

-- | Map a function over a vector and concatenate the results.
concatMap :: (Vector v a, Vector v b) => (a -> v b) -> v a -> v b
{-# INLINE concatMap #-}
-- NOTE: We can't fuse concatMap anyway so don't pretend we do.
-- This seems to be slightly slower
-- concatMap f = concat . Stream.toList . Stream.map f . stream

-- Slowest
-- concatMap f = unstream . Stream.concatMap (stream . f) . stream

-- Seems to be fastest
concatMap f = unstream
            . Stream.flatten mk step Unknown
            . stream
  where
    {-# INLINE_INNER step #-}
    step (v,i,k)
      | i < k = case unsafeIndexM v i of
                  Box x -> Stream.Yield x (v,i+1,k)
      | otherwise = Stream.Done

    {-# INLINE mk #-}
    mk x = let v = f x
               k = length v
           in
           k `seq` (v,0,k)

-}

-- Monadic mapping
-- ---------------

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results
mapM :: (Monad m, Streamable v, Streamable v') => (Element v -> m (Element v')) -> v -> m v'
{-# INLINE mapM #-}
mapM f = unstreamM . Stream.mapM f . stream

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results
mapM_ :: (Monad m, Streamable v) => (Element v -> m a) -> v -> m ()
{-# INLINE mapM_ #-}
mapM_ f = Stream.mapM_ f . stream

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results. Equvalent to @flip 'mapM'@.
forM :: (Monad m, Streamable v, Streamable v') => v -> (Element v -> m (Element v')) -> m v'
{-# INLINE forM #-}
forM as f = mapM f as

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results. Equivalent to @flip 'mapM_'@.
forM_ :: (Monad m, Streamable v) => v -> (Element v -> m a) -> m ()
{-# INLINE forM_ #-}
forM_ as f = mapM_ f as

-- Zipping
-- -------

-- | /O(min(m,n))/ Zip two vectors with the given function.
zipWith :: (Streamable va, Streamable vb, Streamable vc)
        => (Element va -> Element vb -> Element vc) -> va -> vb -> vc
{-# INLINE zipWith #-}
zipWith f xs ys = unstream (Stream.zipWith f (stream xs) (stream ys))

-- | Zip three vectors with the given function.
zipWith3 :: (Streamable va, Streamable vb, Streamable vc, Streamable vd)
         => (Element va -> Element vb -> Element vc -> Element vd) -> va -> vb -> vc -> vd
{-# INLINE zipWith3 #-}
zipWith3 f as bs cs = unstream (Stream.zipWith3 f (stream as)
                                                  (stream bs)
                                                  (stream cs))

zipWith4 :: (Streamable va, Streamable vb, Streamable vc, Streamable vd, Streamable ve)
         => (Element va -> Element vb -> Element vc -> Element vd -> Element ve) -> va -> vb -> vc -> vd -> ve
{-# INLINE zipWith4 #-}
zipWith4 f as bs cs ds
  = unstream (Stream.zipWith4 f (stream as)
                                (stream bs)
                                (stream cs)
                                (stream ds))

zipWith5 :: (Streamable va, Streamable vb, Streamable vc, Streamable vd, Streamable ve,
             Streamable vf)
         => (Element va -> Element vb -> Element vc -> Element vd -> Element ve -> Element vf) -> va -> vb -> vc -> vd -> ve -> vf
{-# INLINE zipWith5 #-}
zipWith5 f as bs cs ds es
  = unstream (Stream.zipWith5 f (stream as)
                                (stream bs)
                                (stream cs)
                                (stream ds)
                                (stream es))

zipWith6 :: (Streamable va, Streamable vb, Streamable vc, Streamable vd, Streamable ve,
             Streamable vf, Streamable vg)
         => (Element va -> Element vb -> Element vc -> Element vd -> Element ve -> Element vf -> Element vg)
         -> va -> vb -> vc -> vd -> ve -> vf -> vg
{-# INLINE zipWith6 #-}
zipWith6 f as bs cs ds es fs
  = unstream (Stream.zipWith6 f (stream as)
                                (stream bs)
                                (stream cs)
                                (stream ds)
                                (stream es)
                                (stream fs))

{-
-- | /O(min(m,n))/ Zip two vectors with a function that also takes the
-- elements' indices.
izipWith :: (Vector v a, Vector v b, Vector v c)
        => (Int -> a -> b -> c) -> v a -> v b -> v c
{-# INLINE izipWith #-}
izipWith f xs ys = unstream
                  (Stream.zipWith (uncurry f) (Stream.indexed (stream xs))
                                                              (stream ys))

izipWith3 :: (Vector v a, Vector v b, Vector v c, Vector v d)
         => (Int -> a -> b -> c -> d) -> v a -> v b -> v c -> v d
{-# INLINE izipWith3 #-}
izipWith3 f as bs cs
  = unstream (Stream.zipWith3 (uncurry f) (Stream.indexed (stream as))
                                                          (stream bs)
                                                          (stream cs))

izipWith4 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e)
         => (Int -> a -> b -> c -> d -> e) -> v a -> v b -> v c -> v d -> v e
{-# INLINE izipWith4 #-}
izipWith4 f as bs cs ds
  = unstream (Stream.zipWith4 (uncurry f) (Stream.indexed (stream as))
                                                          (stream bs)
                                                          (stream cs)
                                                          (stream ds))

izipWith5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
             Vector v f)
         => (Int -> a -> b -> c -> d -> e -> f) -> v a -> v b -> v c -> v d
                                                -> v e -> v f
{-# INLINE izipWith5 #-}
izipWith5 f as bs cs ds es
  = unstream (Stream.zipWith5 (uncurry f) (Stream.indexed (stream as))
                                                          (stream bs)
                                                          (stream cs)
                                                          (stream ds)
                                                          (stream es))

izipWith6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
             Vector v f, Vector v g)
         => (Int -> a -> b -> c -> d -> e -> f -> g)
         -> v a -> v b -> v c -> v d -> v e -> v f -> v g
{-# INLINE izipWith6 #-}
izipWith6 f as bs cs ds es fs
  = unstream (Stream.zipWith6 (uncurry f) (Stream.indexed (stream as))
                                                          (stream bs)
                                                          (stream cs)
                                                          (stream ds)
                                                          (stream es)
                                                          (stream fs))

-- | /O(min(m,n))/ Zip two vectors
zip :: (Vector v a, Vector v b, Vector v (a,b)) => v a -> v b -> v (a, b)
{-# INLINE zip #-}
zip = zipWith (,)

zip3 :: (Vector v a, Vector v b, Vector v c, Vector v (a, b, c))
     => v a -> v b -> v c -> v (a, b, c)
{-# INLINE zip3 #-}
zip3 = zipWith3 (,,)

zip4 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v (a, b, c, d))
     => v a -> v b -> v c -> v d -> v (a, b, c, d)
{-# INLINE zip4 #-}
zip4 = zipWith4 (,,,)

zip5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
         Vector v (a, b, c, d, e))
     => v a -> v b -> v c -> v d -> v e -> v (a, b, c, d, e)
{-# INLINE zip5 #-}
zip5 = zipWith5 (,,,,)

zip6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
         Vector v f, Vector v (a, b, c, d, e, f))
     => v a -> v b -> v c -> v d -> v e -> v f -> v (a, b, c, d, e, f)
{-# INLINE zip6 #-}
zip6 = zipWith6 (,,,,,)

-- Monadic zipping
-- ---------------

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and yield a
-- vector of results
zipWithM :: (Monad m, Vector v a, Vector v b, Vector v c)
         => (a -> b -> m c) -> v a -> v b -> m (v c)
-- FIXME: specialise for ST and IO?
{-# INLINE zipWithM #-}
zipWithM f as bs = unstreamM $ Stream.zipWithM f (stream as) (stream bs)

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and ignore the
-- results
zipWithM_ :: (Monad m, Vector v a, Vector v b)
          => (a -> b -> m c) -> v a -> v b -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ f as bs = Stream.zipWithM_ f (stream as) (stream bs)

-- Unzipping
-- ---------

-- | /O(min(m,n))/ Unzip a vector of pairs.
unzip :: (Vector v a, Vector v b, Vector v (a,b)) => v (a, b) -> (v a, v b)
{-# INLINE unzip #-}
unzip xs = (map fst xs, map snd xs)

unzip3 :: (Vector v a, Vector v b, Vector v c, Vector v (a, b, c))
       => v (a, b, c) -> (v a, v b, v c)
{-# INLINE unzip3 #-}
unzip3 xs = (map (\(a, b, c) -> a) xs,
             map (\(a, b, c) -> b) xs,
             map (\(a, b, c) -> c) xs)

unzip4 :: (Vector v a, Vector v b, Vector v c, Vector v d,
           Vector v (a, b, c, d))
       => v (a, b, c, d) -> (v a, v b, v c, v d)
{-# INLINE unzip4 #-}
unzip4 xs = (map (\(a, b, c, d) -> a) xs,
             map (\(a, b, c, d) -> b) xs,
             map (\(a, b, c, d) -> c) xs,
             map (\(a, b, c, d) -> d) xs)

unzip5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
           Vector v (a, b, c, d, e))
       => v (a, b, c, d, e) -> (v a, v b, v c, v d, v e)
{-# INLINE unzip5 #-}
unzip5 xs = (map (\(a, b, c, d, e) -> a) xs,
             map (\(a, b, c, d, e) -> b) xs,
             map (\(a, b, c, d, e) -> c) xs,
             map (\(a, b, c, d, e) -> d) xs,
             map (\(a, b, c, d, e) -> e) xs)

unzip6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
           Vector v f, Vector v (a, b, c, d, e, f))
       => v (a, b, c, d, e, f) -> (v a, v b, v c, v d, v e, v f)
{-# INLINE unzip6 #-}
unzip6 xs = (map (\(a, b, c, d, e, f) -> a) xs,
             map (\(a, b, c, d, e, f) -> b) xs,
             map (\(a, b, c, d, e, f) -> c) xs,
             map (\(a, b, c, d, e, f) -> d) xs,
             map (\(a, b, c, d, e, f) -> e) xs,
             map (\(a, b, c, d, e, f) -> f) xs)

-}

-- Filtering
-- ---------

-- | /O(n)/ Drop elements that do not satisfy the predicate
filter :: Streamable v => (Element v -> Bool) -> v -> v
{-# INLINE filter #-}
filter f = unstream . inplace (MStream.filter f) . stream

-- | /O(n)/ Drop elements that do not satisfy the predicate which is applied to
-- values and their indices
ifilter :: Vector v a => (Int -> a -> Bool) -> v a -> v a
{-# INLINE ifilter #-}
ifilter f = unstream
          . inplace (MStream.map snd . MStream.filter (uncurry f)
                                     . MStream.indexed)
          . stream

-- | /O(n)/ Drop elements that do not satisfy the monadic predicate
filterM :: (Monad m, Vector v a) => (a -> m Bool) -> v a -> m (v a)
{-# INLINE filterM #-}
filterM f = unstreamM . Stream.filterM f . stream

-- | /O(n)/ Yield the longest prefix of elements satisfying the predicate
-- without copying.
takeWhile :: Vector v a => (a -> Bool) -> v a -> v a
{-# INLINE takeWhile #-}
takeWhile f = unstream . Stream.takeWhile f . stream

-- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- without copying.
dropWhile :: Vector v a => (a -> Bool) -> v a -> v a
{-# INLINE dropWhile #-}
dropWhile f = unstream . Stream.dropWhile f . stream

-- Parititioning
-- -------------

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't. The
-- relative order of the elements is preserved at the cost of a sometimes
-- reduced performance compared to 'unstablePartition'.
partition :: Vector v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE partition #-}
partition f = partition_stream f . stream

-- FIXME: Make this inplace-fusible (look at how stable_partition is
-- implemented in C++)

partition_stream :: Vector v a => (a -> Bool) -> Stream a -> (v a, v a)
{-# INLINE_STREAM partition_stream #-}
partition_stream f s = s `seq` runST (
  do
    (mv1,mv2) <- M.partitionStream f s
    v1 <- unsafeFreeze mv1
    v2 <- unsafeFreeze mv2
    return (v1,v2))

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't.
-- The order of the elements is not preserved but the operation is often
-- faster than 'partition'.
unstablePartition :: Vector v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE unstablePartition #-}
unstablePartition f = unstablePartition_stream f . stream

unstablePartition_stream
  :: Vector v a => (a -> Bool) -> Stream a -> (v a, v a)
{-# INLINE_STREAM unstablePartition_stream #-}
unstablePartition_stream f s = s `seq` runST (
  do
    (mv1,mv2) <- M.unstablePartitionStream f s
    v1 <- unsafeFreeze mv1
    v2 <- unsafeFreeze mv2
    return (v1,v2))

unstablePartition_new :: Vector v a => (a -> Bool) -> New v a -> (v a, v a)
{-# INLINE_STREAM unstablePartition_new #-}
unstablePartition_new f (New.New p) = runST (
  do
    mv <- p
    i <- M.unstablePartition f mv
    v <- unsafeFreeze mv
    return (unsafeTake i v, unsafeDrop i v))

{-# RULES

"unstablePartition" forall f p.
  unstablePartition_stream f (stream (new p))
    = unstablePartition_new f p

  #-}


-- FIXME: make span and break fusible

-- | /O(n)/ Split the vector into the longest prefix of elements that satisfy
-- the predicate and the rest without copying.
span :: Vector v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE span #-}
span f = break (not . f)

-- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest without copying.
break :: Vector v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE break #-}
break f xs = case findIndex f xs of
               Just i  -> (unsafeSlice 0 i xs, unsafeSlice i (length xs - i) xs)
               Nothing -> (xs, empty)
    

-- Searching
-- ---------

infix 4 `elem`
-- | /O(n)/ Check if the vector contains an element
elem :: (Vector v a, Eq a) => a -> v a -> Bool
{-# INLINE elem #-}
elem x = Stream.elem x . stream

infix 4 `notElem`
-- | /O(n)/ Check if the vector does not contain an element (inverse of 'elem')
notElem :: (Vector v a, Eq a) => a -> v a -> Bool
{-# INLINE notElem #-}
notElem x = Stream.notElem x . stream

-- | /O(n)/ Yield 'Just' the first element matching the predicate or 'Nothing'
-- if no such element exists.
find :: Vector v a => (a -> Bool) -> v a -> Maybe a
{-# INLINE find #-}
find f = Stream.find f . stream

-- | /O(n)/ Yield 'Just' the index of the first element matching the predicate
-- or 'Nothing' if no such element exists.
findIndex :: Vector v a => (a -> Bool) -> v a -> Maybe Int
{-# INLINE findIndex #-}
findIndex f = Stream.findIndex f . stream

-- | /O(n)/ Yield the indices of elements satisfying the predicate in ascending
-- order.
findIndices :: (Vector v a, Vector v Int) => (a -> Bool) -> v a -> v Int
{-# INLINE findIndices #-}
findIndices f = unstream
              . inplace (MStream.map fst . MStream.filter (f . snd)
                                         . MStream.indexed)
              . stream

-- | /O(n)/ Yield 'Just' the index of the first occurence of the given element or
-- 'Nothing' if the vector does not contain the element. This is a specialised
-- version of 'findIndex'.
elemIndex :: (Vector v a, Eq a) => a -> v a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex x = findIndex (x==)

-- | /O(n)/ Yield the indices of all occurences of the given element in
-- ascending order. This is a specialised version of 'findIndices'.
elemIndices :: (Vector v a, Vector v Int, Eq a) => a -> v a -> v Int
{-# INLINE elemIndices #-}
elemIndices x = findIndices (x==)

-- Folding
-- -------

-- | /O(n)/ Left fold
foldl :: Vector v b => (a -> b -> a) -> a -> v b -> a
{-# INLINE foldl #-}
foldl f z = Stream.foldl f z . stream

-- | /O(n)/ Left fold on non-empty vectors
foldl1 :: Vector v a => (a -> a -> a) -> v a -> a
{-# INLINE foldl1 #-}
foldl1 f = Stream.foldl1 f . stream

-- | /O(n)/ Left fold with strict accumulator
foldl' :: Vector v b => (a -> b -> a) -> a -> v b -> a
{-# INLINE foldl' #-}
foldl' f z = Stream.foldl' f z . stream

-- | /O(n)/ Left fold on non-empty vectors with strict accumulator
foldl1' :: Vector v a => (a -> a -> a) -> v a -> a
{-# INLINE foldl1' #-}
foldl1' f = Stream.foldl1' f . stream

-- | /O(n)/ Right fold
foldr :: Vector v a => (a -> b -> b) -> b -> v a -> b
{-# INLINE foldr #-}
foldr f z = Stream.foldr f z . stream

-- | /O(n)/ Right fold on non-empty vectors
foldr1 :: Vector v a => (a -> a -> a) -> v a -> a
{-# INLINE foldr1 #-}
foldr1 f = Stream.foldr1 f . stream

-- | /O(n)/ Right fold with a strict accumulator
foldr' :: Vector v a => (a -> b -> b) -> b -> v a -> b
{-# INLINE foldr' #-}
foldr' f z = Stream.foldl' (flip f) z . streamR

-- | /O(n)/ Right fold on non-empty vectors with strict accumulator
foldr1' :: Vector v a => (a -> a -> a) -> v a -> a
{-# INLINE foldr1' #-}
foldr1' f = Stream.foldl1' (flip f) . streamR

-- | /O(n)/ Left fold (function applied to each element and its index)
ifoldl :: Vector v b => (a -> Int -> b -> a) -> a -> v b -> a
{-# INLINE ifoldl #-}
ifoldl f z = Stream.foldl (uncurry . f) z . Stream.indexed . stream

-- | /O(n)/ Left fold with strict accumulator (function applied to each element
-- and its index)
ifoldl' :: Vector v b => (a -> Int -> b -> a) -> a -> v b -> a
{-# INLINE ifoldl' #-}
ifoldl' f z = Stream.foldl' (uncurry . f) z . Stream.indexed . stream

-- | /O(n)/ Right fold (function applied to each element and its index)
ifoldr :: Vector v a => (Int -> a -> b -> b) -> b -> v a -> b
{-# INLINE ifoldr #-}
ifoldr f z = Stream.foldr (uncurry f) z . Stream.indexed . stream

-- | /O(n)/ Right fold with strict accumulator (function applied to each
-- element and its index)
ifoldr' :: Vector v a => (Int -> a -> b -> b) -> b -> v a -> b
{-# INLINE ifoldr' #-}
ifoldr' f z xs = Stream.foldl' (flip (uncurry f)) z
               $ Stream.indexedR (length xs) $ streamR xs

-- Specialised folds
-- -----------------

-- | /O(n)/ Check if all elements satisfy the predicate.
all :: Vector v a => (a -> Bool) -> v a -> Bool
{-# INLINE all #-}
all f = Stream.and . Stream.map f . stream

-- | /O(n)/ Check if any element satisfies the predicate.
any :: Vector v a => (a -> Bool) -> v a -> Bool
{-# INLINE any #-}
any f = Stream.or . Stream.map f . stream

-- | /O(n)/ Check if all elements are 'True'
and :: Vector v Bool => v Bool -> Bool
{-# INLINE and #-}
and = Stream.and . stream

-- | /O(n)/ Check if any element is 'True'
or :: Vector v Bool => v Bool -> Bool
{-# INLINE or #-}
or = Stream.or . stream

-- | /O(n)/ Compute the sum of the elements
sum :: (Vector v a, Num a) => v a -> a
{-# INLINE sum #-}
sum = Stream.foldl' (+) 0 . stream

-- | /O(n)/ Compute the produce of the elements
product :: (Vector v a, Num a) => v a -> a
{-# INLINE product #-}
product = Stream.foldl' (*) 1 . stream

-- | /O(n)/ Yield the maximum element of the vector. The vector may not be
-- empty.
maximum :: (Vector v a, Ord a) => v a -> a
{-# INLINE maximum #-}
maximum = Stream.foldl1' max . stream

-- | /O(n)/ Yield the maximum element of the vector according to the given
-- comparison function. The vector may not be empty.
maximumBy :: Vector v a => (a -> a -> Ordering) -> v a -> a
{-# INLINE maximumBy #-}
maximumBy cmp = Stream.foldl1' maxBy . stream
  where
    {-# INLINE maxBy #-}
    maxBy x y = case cmp x y of
                  LT -> y
                  _  -> x

-- | /O(n)/ Yield the minimum element of the vector. The vector may not be
-- empty.
minimum :: (Vector v a, Ord a) => v a -> a
{-# INLINE minimum #-}
minimum = Stream.foldl1' min . stream

-- | /O(n)/ Yield the minimum element of the vector according to the given
-- comparison function. The vector may not be empty.
minimumBy :: Vector v a => (a -> a -> Ordering) -> v a -> a
{-# INLINE minimumBy #-}
minimumBy cmp = Stream.foldl1' minBy . stream
  where
    {-# INLINE minBy #-}
    minBy x y = case cmp x y of
                  GT -> y
                  _  -> x

-- | /O(n)/ Yield the index of the maximum element of the vector. The vector
-- may not be empty.
maxIndex :: (Vector v a, Ord a) => v a -> Int
{-# INLINE maxIndex #-}
maxIndex = maxIndexBy compare

-- | /O(n)/ Yield the index of the maximum element of the vector according to
-- the given comparison function. The vector may not be empty.
maxIndexBy :: Vector v a => (a -> a -> Ordering) -> v a -> Int
{-# INLINE maxIndexBy #-}
maxIndexBy cmp = fst . Stream.foldl1' imax . Stream.indexed . stream
  where
    imax (i,x) (j,y) = i `seq` j `seq`
                       case cmp x y of
                         LT -> (j,y)
                         _  -> (i,x)

-- | /O(n)/ Yield the index of the minimum element of the vector. The vector
-- may not be empty.
minIndex :: (Vector v a, Ord a) => v a -> Int
{-# INLINE minIndex #-}
minIndex = minIndexBy compare

-- | /O(n)/ Yield the index of the minimum element of the vector according to
-- the given comparison function. The vector may not be empty.
minIndexBy :: Vector v a => (a -> a -> Ordering) -> v a -> Int
{-# INLINE minIndexBy #-}
minIndexBy cmp = fst . Stream.foldl1' imin . Stream.indexed . stream
  where
    imin (i,x) (j,y) = i `seq` j `seq`
                       case cmp x y of
                         GT -> (j,y)
                         _  -> (i,x)

-- Monadic folds
-- -------------

-- | /O(n)/ Monadic fold
foldM :: (Monad m, Vector v b) => (a -> b -> m a) -> a -> v b -> m a
{-# INLINE foldM #-}
foldM m z = Stream.foldM m z . stream

-- | /O(n)/ Monadic fold over non-empty vectors
fold1M :: (Monad m, Vector v a) => (a -> a -> m a) -> v a -> m a
{-# INLINE fold1M #-}
fold1M m = Stream.fold1M m . stream

-- | /O(n)/ Monadic fold with strict accumulator
foldM' :: (Monad m, Vector v b) => (a -> b -> m a) -> a -> v b -> m a
{-# INLINE foldM' #-}
foldM' m z = Stream.foldM' m z . stream

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
fold1M' :: (Monad m, Vector v a) => (a -> a -> m a) -> v a -> m a
{-# INLINE fold1M' #-}
fold1M' m = Stream.fold1M' m . stream

discard :: Monad m => m a -> m ()
{-# INLINE discard #-}
discard m = m >> return ()

-- | /O(n)/ Monadic fold that discards the result
foldM_ :: (Monad m, Vector v b) => (a -> b -> m a) -> a -> v b -> m ()
{-# INLINE foldM_ #-}
foldM_ m z = discard . Stream.foldM m z . stream

-- | /O(n)/ Monadic fold over non-empty vectors that discards the result
fold1M_ :: (Monad m, Vector v a) => (a -> a -> m a) -> v a -> m ()
{-# INLINE fold1M_ #-}
fold1M_ m = discard . Stream.fold1M m . stream

-- | /O(n)/ Monadic fold with strict accumulator that discards the result
foldM'_ :: (Monad m, Vector v b) => (a -> b -> m a) -> a -> v b -> m ()
{-# INLINE foldM'_ #-}
foldM'_ m z = discard . Stream.foldM' m z . stream

-- | /O(n)/ Monad fold over non-empty vectors with strict accumulator
-- that discards the result
fold1M'_ :: (Monad m, Vector v a) => (a -> a -> m a) -> v a -> m ()
{-# INLINE fold1M'_ #-}
fold1M'_ m = discard . Stream.fold1M' m . stream

-- Monadic sequencing
-- ------------------

-- | Evaluate each action and collect the results
sequence :: (Monad m, Vector v a, Vector v (m a)) => v (m a) -> m (v a)
{-# INLINE sequence #-}
sequence = mapM id

-- | Evaluate each action and discard the results
sequence_ :: (Monad m, Vector v (m a)) => v (m a) -> m ()
{-# INLINE sequence_ #-}
sequence_ = mapM_ id

-- Prefix sums (scans)
-- -------------------

-- | /O(n)/ Prescan
--
-- @
-- prescanl f z = 'init' . 'scanl' f z
-- @
--
-- Example: @prescanl (+) 0 \<1,2,3,4\> = \<0,1,3,6\>@
--
prescanl :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE prescanl #-}
prescanl f z = unstream . inplace (MStream.prescanl f z) . stream

-- | /O(n)/ Prescan with strict accumulator
prescanl' :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE prescanl' #-}
prescanl' f z = unstream . inplace (MStream.prescanl' f z) . stream

-- | /O(n)/ Scan
--
-- @
-- postscanl f z = 'tail' . 'scanl' f z
-- @
--
-- Example: @postscanl (+) 0 \<1,2,3,4\> = \<1,3,6,10\>@
--
postscanl :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE postscanl #-}
postscanl f z = unstream . inplace (MStream.postscanl f z) . stream

-- | /O(n)/ Scan with strict accumulator
postscanl' :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE postscanl' #-}
postscanl' f z = unstream . inplace (MStream.postscanl' f z) . stream

-- | /O(n)/ Haskell-style scan
--
-- > scanl f z <x1,...,xn> = <y1,...,y(n+1)>
-- >   where y1 = z
-- >         yi = f y(i-1) x(i-1)
--
-- Example: @scanl (+) 0 \<1,2,3,4\> = \<0,1,3,6,10\>@
-- 
scanl :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE scanl #-}
scanl f z = unstream . Stream.scanl f z . stream

-- | /O(n)/ Haskell-style scan with strict accumulator
scanl' :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE scanl' #-}
scanl' f z = unstream . Stream.scanl' f z . stream

-- | /O(n)/ Scan over a non-empty vector
--
-- > scanl f <x1,...,xn> = <y1,...,yn>
-- >   where y1 = x1
-- >         yi = f y(i-1) xi
--
scanl1 :: Vector v a => (a -> a -> a) -> v a -> v a
{-# INLINE scanl1 #-}
scanl1 f = unstream . inplace (MStream.scanl1 f) . stream

-- | /O(n)/ Scan over a non-empty vector with a strict accumulator
scanl1' :: Vector v a => (a -> a -> a) -> v a -> v a
{-# INLINE scanl1' #-}
scanl1' f = unstream . inplace (MStream.scanl1' f) . stream

-- | /O(n)/ Right-to-left prescan
--
-- @
-- prescanr f z = 'reverse' . 'prescanl' (flip f) z . 'reverse'
-- @
--
prescanr :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> v a -> v b
{-# INLINE prescanr #-}
prescanr f z = unstreamR . inplace (MStream.prescanl (flip f) z) . streamR

-- | /O(n)/ Right-to-left prescan with strict accumulator
prescanr' :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> v a -> v b
{-# INLINE prescanr' #-}
prescanr' f z = unstreamR . inplace (MStream.prescanl' (flip f) z) . streamR

-- | /O(n)/ Right-to-left scan
postscanr :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> v a -> v b
{-# INLINE postscanr #-}
postscanr f z = unstreamR . inplace (MStream.postscanl (flip f) z) . streamR

-- | /O(n)/ Right-to-left scan with strict accumulator
postscanr' :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> v a -> v b
{-# INLINE postscanr' #-}
postscanr' f z = unstreamR . inplace (MStream.postscanl' (flip f) z) . streamR

-- | /O(n)/ Right-to-left Haskell-style scan
scanr :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> v a -> v b
{-# INLINE scanr #-}
scanr f z = unstreamR . Stream.scanl (flip f) z . streamR

-- | /O(n)/ Right-to-left Haskell-style scan with strict accumulator
scanr' :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> v a -> v b
{-# INLINE scanr' #-}
scanr' f z = unstreamR . Stream.scanl' (flip f) z . streamR

-- | /O(n)/ Right-to-left scan over a non-empty vector
scanr1 :: Vector v a => (a -> a -> a) -> v a -> v a
{-# INLINE scanr1 #-}
scanr1 f = unstreamR . inplace (MStream.scanl1 (flip f)) . streamR

-- | /O(n)/ Right-to-left scan over a non-empty vector with a strict
-- accumulator
scanr1' :: Vector v a => (a -> a -> a) -> v a -> v a
{-# INLINE scanr1' #-}
scanr1' f = unstreamR . inplace (MStream.scanl1' (flip f)) . streamR
