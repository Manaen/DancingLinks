import Data.Array.Unboxed (UArray, (!))
import Control.Monad.ST.Lazy
import Control.Monad (replicateM, forM_, when)
import Data.List
import Debug.Trace(trace,traceShowM)
import Data.STRef.Lazy
import Data.Array.ST (STArray, newArray, readArray, writeArray,getBounds)
import Data.Array.IArray (listArray)
import System.IO
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad.Loops-- (whileM)
--import Control.Monad.ST.Unsafe
import Control.Monad.ST.Lazy.Unsafe (unsafeIOToST)


data Node s = Node { up, down,itm :: STRef s  Int} |Null 

data Item  s = Item { prev, next,numNodes :: STRef s Int,
                     name ::STRef s String } |Nill 

instance Eq (Node s) where
	a==b = (up a == up b) && (down a == down b)&&(itm a == itm b)

 {-
instance Show (Node s ) where
	showsPrec i (Node {up=u,down=d,itm=t} ) s = show  u ++""
		where (u,d,t) = unsafePerformIO $ do 
			u<-stToIO $ readSTRef u
			d<- stToIO $ readSTRef d 
			t<-stToIO $ readSTRef t
			return(u,d,t)
-}

nods = newArray (0,30) Null :: ST s (STArray s Int (Node s))
itms = newArray (0,10) Nill :: ST s (STArray s Int (Item s))

addNode :: Int->Int ->Int  ->STArray s Int (Node s)->Int->ST s ()
addNode u d i arr pos = do 
	nd <- newNode u d i 
	writeArray arr pos nd 

addItem:: Int -> Int -> Int -> String->STArray s Int (Item s)->Int->ST s ()
addItem pr nx sm nm col pos = do 
	it <- newItem pr nx sm nm 
	writeArray col pos it 
{--
test = do 
	pp<-stToIO $ nods
	its <-stToIO $ itms

	stToIO $ addItem 7 1 0 "0" its 0
	stToIO $ addItem 0 2 2 "a" its 1
	stToIO $ addItem 1 3 2 "b" its 2
	stToIO $ addItem 2 4 2 "c" its 3
	stToIO $ addItem 3 5 3 "d" its 4
	stToIO $ addItem 4 6 2 "e" its 5
	stToIO $ addItem 5 7 2 "f" its 6
	stToIO $ addItem 6 0 3 "g" its 7

	stToIO $ addNode 0 0 0 pp 0
	stToIO $ addNode 20 12 1  pp 1
	stToIO $ addNode 24 16  2 pp 2
	stToIO $ addNode 17 9 3 pp 3
	stToIO $ addNode 27 13 4 pp 4
	stToIO $ addNode 28 10 5 pp 5
	stToIO $ addNode 22 18 6 pp 6
	stToIO $ addNode 29 14 7 pp 7
	stToIO $ addNode 0 10 0 pp 8
	stToIO $ addNode 3 17 3 pp 9
	stToIO $ addNode 5 28 5 pp 10
	stToIO $ addNode 9 14 (-1) pp 11
	stToIO $ addNode 1 20 1 pp 12 
	stToIO $ addNode 4 21 4 pp 13
	stToIO $ addNode 7 25 7 pp 14
	stToIO $ addNode 12 18 (-2) pp 15
	stToIO $ addNode 2 24 2 pp 16
	stToIO $ addNode 9 3 3 pp 17
	stToIO $ addNode 6 22 6 pp 18
	stToIO $ addNode 16 22 (-3) pp 19
	stToIO $ addNode 12 1 1 pp 20
	stToIO $ addNode 13 27 4 pp 21
	stToIO $ addNode  18 6 6 pp 22
	stToIO $ addNode 20 25 (-4) pp 23
	stToIO $ addNode 16 2 2 pp 24
	stToIO $ addNode 14 29 7 pp 25
	stToIO $ addNode 24 29 (-5) pp 26
	stToIO $ addNode 21 4 4 pp 27
	stToIO $ addNode 10 5 5 pp 28
	stToIO $ addNode 25 7 7 pp 29
	stToIO $ addNode 27 0 (-6) pp 30

	nList<-stToIO $ readNodes pp
	iList <- stToIO$ readItems its 

	putStrLn ("All nodes = " ++ show nList)
	putStrLn ("All items = " ++ show iList)

	stToIO$ cover 4  pp its 
	putStrLn(" \n")

	nList2<-stToIO $ readNodes pp
	iList2 <- stToIO$ readItems its  

	putStrLn ("All nodes = " ++ show nList2)
	putStrLn ("All items = " ++ show iList2)

	stToIO$ uncover 4 pp its
	putStrLn ("\n")

	nList3<-stToIO $ readNodes pp
	iList3 <- stToIO$ readItems its

	 


	

	putStrLn ("All nodes = " ++ show nList3)
	putStrLn ("All items = " ++ show iList3)

	it <- stToIO $ choseIt its
	putStrLn ("best " ++ show it)

	q<- stToIO $ findPosition "f" its 
	putStrLn ("position "++show q)
--}

findPosition :: String -> STArray s Int (Item s )->ST s Int
findPosition nm cols = do 
	curi <- readArray cols 0
	--traceShowM nm
	p <- getAttr' next curi
	--traceShowM p
	pos <- newSTRef p 
	whileM(
		do 
			p <- readSTRef pos
			c <- readArray cols p
			nam <- getAttr' name c
			return (p/=0&&nm/=""&&nm/=nam)
		)(
		do 

			p<- readSTRef pos 
			--traceShowM p
			c <- readArray cols p 
			nxt <- getAttr' next c 
			writeSTRef pos nxt
		)
	p<- readSTRef pos
	return p




printNode node = do 
	u <-stToIO $ getAttr up node
	d <-stToIO $ getAttr down node
	i <-stToIO $ getAttr itm node
	print u 
	print d 
	print i  

printAllNodes:: STArray s Int (Node s) -> ST s [()]
printAllNodes arrayNode = do 
	i <- newSTRef 0 
	whileM (
		do  
			ii <- readSTRef i 
			x <- readArray arrayNode ii 
			return (x/= Null)
			) ( do 
					y <- readSTRef i 
					x <-  readArray arrayNode y
					xx <- getAttr up x 
					traceShowM xx------------------------------------------
					--xxx <- unsafeIOToST $ print xx
					modifySTRef i (+1)

					)


readNodes :: STArray s Int (Node s )->ST s [(Int,Int,Int)]
readNodes nodes = do 
	(lower, upper) <- getBounds nodes
	nods <- mapM (\i -> readArray nodes i) [lower..upper-1]
	xx <- mapM ( \i -> f i (nods !! i)) [0..upper-lower-1]
	return xx
	   where 
	   	    f _ Null = return (0, 0,0)
	   	    f j (Node {up=u, down=d, itm = i}) = 	   		
	   	      do
	   	        u <- readSTRef u 
	   	        d <- readSTRef d 
	   	        --i <- readSTRef i 
	   	     	return (u,d,j)

readItems :: STArray s Int (Item s )->ST s [(String,Int,Int,Int)]
readItems items = do 
	(lower, upper) <- getBounds items
	itms <- mapM (\i -> readArray items i) [lower..upper-1]
	xx <- mapM ( \i -> f (itms !! i)) [0..upper-lower-1]
	return xx
	   where 
	   	    f Nill = return ("-",0, 0,0)
	   	    f (Item {prev=pr, next=nx, numNodes = sm,name=nm  }) = 	   		
	   	      do
	   	        pr <- readSTRef pr 
	   	        nx <- readSTRef nx 
	   	        sm <- readSTRef sm 
	   	        nm <- readSTRef nm 
	   	        return (nm,pr,nx,sm) 	 

	


data Second s = Second {val :: STRef s Int} 
data LastItem s = LastItem{ lst:: STRef s Int} 

maxCol =10000::Int 
maxRow =10000::Int





newNode u d i =
        do 
		 uu <- newSTRef u
		 dd <- newSTRef d
		 ii <- newSTRef i
		 return ( Node uu dd ii )


newItem p n l nm  =
        do 
		 pp <- newSTRef p
		 nn <- newSTRef n
		 ll <- newSTRef l
		 nmm <-newSTRef nm
		 return ( Item pp nn ll nmm )


getAttr :: (Node s -> STRef s a) -> Node s -> ST s a
getAttr dir node = readSTRef (dir node)
 
setAttr :: (Node s -> STRef s a) -> Node s -> a -> ST s ()
setAttr dir node = writeSTRef (dir node)

getAttr' :: (Item s -> STRef s a) ->Item s -> ST s a
getAttr' dir item = readSTRef (dir item)
 
setAttr' :: (Item s -> STRef s a) -> Item s -> a -> ST s ()
setAttr' dir item = writeSTRef (dir item) 



hide:: Int->STArray s Int (Node s)->STArray s Int (Item s) ->ST s [()]
hide p nods cols  = 
	do 
		
		q<- newSTRef (p+1) 
		--traceShowM p
		whileM ( do 
			q'<- readSTRef q
			--traceShowM q'
			return(q'/=p)) (
			do 
				q'<-readSTRef q 
				xx<- readArray nods q'
				x <- getAttr itm xx

				u <- getAttr up xx

				d <- getAttr down xx

				if(x<=0) then writeSTRef q u 
				else 
				    do 
				     uu <- readArray nods u 
				     dd <- readArray nods d 

				     setAttr down uu d  
				     setAttr up dd u
				     cc <- readArray cols x 
				     c <- getAttr' numNodes cc
				     setAttr' numNodes cc (c-1)
 

				     modifySTRef q (+1)
				    -- z <- readSTRef q 
				     --traceShowM z	


			)

cover :: Int->STArray s Int (Node s)->STArray s Int (Item s) ->ST s ()
cover it nods cols  = 
	do 
		currNode <-  readArray nods it
		nd <- getAttr down currNode
		pos <- newSTRef nd 
		--traceShowM (" covering item "++show it )
		--traceShowM it 
		--unsafeIOToST (myTrace("\ncovering item "++show it))
		
		whileM( do 
			p<- readSTRef pos 
			--traceShowM p
			return (p/=it)
			) (
			do 
			  p<- readSTRef pos
			  --traceShowM " -------------------- "
			  --traceShowM p
			  hide p nods cols
			  --traceShowM "------------"

			  nxNode<-readArray nods p
			  xy<- getAttr down nxNode
			  writeSTRef pos xy

			)
		currIt<- readArray cols it
		l<-getAttr' prev currIt
		r<-getAttr' next currIt

		lItem <- readArray cols l
		rItem <- readArray cols r

		setAttr' next lItem  r
		setAttr' prev rItem l


unhide:: Int->STArray s Int (Node s)->STArray s Int (Item s) ->ST s [()]
unhide p nods cols  = 
	do 
		currNode<- readArray nods p 
		q<- newSTRef (p-1) 
		--traceShowM p
		whileM ( do 
			q'<- readSTRef q
			--traceShowM q'
			return(q'/=p)) (
			do 
				q'<-readSTRef q 
				xx<- readArray nods q'
				x <- getAttr itm xx

				u <- getAttr up xx

				d <- getAttr down xx

				if(x<=0) then writeSTRef q d 
				else 
				    do 
				     uu <- readArray nods u 
				     dd <- readArray nods d 

				     setAttr down uu q'  
				     setAttr up dd q'
				     cc <- readArray cols x 
				     c <- getAttr' numNodes cc
				     setAttr' numNodes cc (c+1)
 

				     modifySTRef q (+(-1))
				    -- z <- readSTRef q 
				     --traceShowM z	


			)


uncover :: Int->STArray s Int (Node s)->STArray s Int (Item s) ->ST s ()
uncover it nods cols  = 
	do 
		currNode <-  readArray nods it
		nd <- getAttr up currNode
		pos <- newSTRef nd 

		--unsafeIOToST (myTrace("\nuncovering item "++show it))
		--traceShowM (" uncovering item "++show it )
		--traceShowM " uncovering item "
		--traceShowM it
		
		whileM( do 
			p<- readSTRef pos 
			--traceShowM p
			return (p/=it)
			) (
			do 
			  p<- readSTRef pos
			  --traceShowM " -------------------- "
			  --traceShowM p
			  unhide p nods cols
			  --traceShowM "------------"

			  nxNode<-readArray nods p
			  xy<- getAttr up nxNode
			  writeSTRef pos xy

			)
		currIt<- readArray cols it
		l<-getAttr' prev currIt
		r<-getAttr' next currIt

		lItem <- readArray cols l
		rItem <- readArray cols r

		setAttr' next lItem  it
		setAttr' prev rItem it

choseIt :: STArray s Int (Item s)->ST s (Int)
choseIt cols = 
	do 
		ii<- readArray cols 0 
		i<- getAttr' next ii 

		m<- newSTRef 100000000
		curi  <- newSTRef i
		best <-  newSTRef 100 
		bestV <- newSTRef 100

		whileM (
			do 
				x <- readSTRef curi
				return (x/=0)

			) (
			do 
				x <- readSTRef curi
				currIt <- readArray cols x
				currNn <- getAttr' numNodes currIt
				nx <- getAttr' next currIt
				bst <- readSTRef best
				v<- readSTRef bestV
				if (currNn < v) then 
				   do 
				     writeSTRef best x
				     writeSTRef bestV currNn
				     --traceShowM "pass"
				else return ()
				writeSTRef curi nx
				--traceShowM x

				)
		bst <- readSTRef best
		--traceShowM " chosen item is "
		--traceShowM bst
		return  bst

initialize = do 
	pp<- newArray (0,30) Null :: ST s (STArray s Int (Node s))
	its <- newArray (0,30) Nill :: ST s (STArray s Int (Item s))

	addItem 7 1 0 "0" its 0
	addItem 0 2 2 "a" its 1
	addItem 1 3 2 "b" its 2
	addItem 2 4 2 "c" its 3
	addItem 3 5 3 "d" its 4
	addItem 4 6 2 "e" its 5
	addItem 5 7 2 "f" its 6
	addItem 6 0 3 "g" its 7

	addNode 0 0 0 pp 0
	addNode 20 12 1  pp 1
	addNode 24 16  2 pp 2
	addNode 17 9 3 pp 3
	addNode 27 13 4 pp 4
	addNode 28 10 5 pp 5
	addNode 22 18 6 pp 6
	addNode 29 14 7 pp 7
	addNode 0 10 0 pp 8
	addNode 3 17 3 pp 9
	addNode 5 28 5 pp 10
	addNode 9 14 (-1) pp 11
	addNode 1 20 1 pp 12 
	addNode 4 21 4 pp 13
	addNode 7 25 7 pp 14
	addNode 12 18 (-2) pp 15
	addNode 2 24 2 pp 16
	addNode 9 3 3 pp 17
	addNode 6 22 6 pp 18
	addNode 16 22 (-3) pp 19
	addNode 12 1 1 pp 20
	addNode 13 27 4 pp 21
	addNode  18 6 6 pp 22
	addNode 20 25 (-4) pp 23
	addNode 16 2 2 pp 24
	addNode 14 29 7 pp 25
	addNode 24 29 (-5) pp 26
	addNode 21 4 4 pp 27
	addNode 10 5 5 pp 28
	addNode 25 7 7 pp 29
	addNode 27 0 (-6) pp 30

	return (pp,its)


trying = do 
	putStrLn " input the file addres: "
	file <- getLine 
	handle <- openFile file ReadMode

	contents <- hGetContents handle
	let list = lines contents
	let y = length list
	let alla  = map(\i-> words (list!!i))[0..y-1]
	--hClose handle

	return alla 

algorithmD:: ST s [()]
algorithmD = do 
	status <- newSTRef True
	isD2 <- newSTRef True
	isD5 <- newSTRef False
	isD6 <- newSTRef False
	isD8 <- newSTRef False

	count <- newSTRef 0
	it <- newSTRef 0 
	xl <- newSTRef 0 
	l <- newSTRef 0 
	ans <- newArray (0,30) 0::ST s (STArray s Int Int)
	let rr =unsafePerformIO trying-------------------------------------------------------------------------------------- 

	(nodes,items)<- yoyo rr--[["a","b","c","d","e","f","g"],["c","e"],["a","d","g"],["b","c","f"],["a","d","f"],["b","g"],["d","e","g"]] 
	
	--(a,b )<- yoyo  rr
	--traceShowM " initialize done" 

	whileM (
		do 
			x<- readSTRef status
			return x 

		)( 
		do 
			d2 <- readSTRef isD2
			
			

			if ( d2 ) then do 
				--unsafeIOToST (myTrace "\n D2")
				--traceShowM " D2 ......."
				nxt <- readArray items 0 
				nx <- getAttr' next nxt 

				if(nx ==0) then do 
					--traceShowM " got an answer"
					--writeSTRef status False
					--let x = unsafePerformIO$ (myTrace "got an answer")
					--traceShowM " go to D8 "
					--printAns ans
					--unsafeIOToST (myTrace "\n got an answer \n go to d8")
					modifySTRef count (+1)
					writeSTRef isD8 True
					anss <- readSTRef count
					traceShowM anss
					--writeSTRef status False
					--writeSTRef isD2 False
				else do
					--traceShowM "nxt"
					x<- choseIt items
					writeSTRef it x 
					xll <- readArray nodes x 
					xll <- getAttr down xll 
					writeSTRef xl xll 
					--traceShowM " xl is now "
					--traceShowM xll 
					--unsafeIOToST (myTrace ("\nxl :"++show xll ++" my it"++show x ))
					cover x nodes items
					--writeSTRef isD2 False
					writeSTRef isD5 True
					--writeSTRef isD8 False
				writeSTRef isD2 False
			else return ()
			d5 <- readSTRef isD5
			
			if(d5) then do 
				--unsafeIOToST (myTrace "\n D5.......")
				--traceShowM "D5 ......"
				myXl<- readSTRef xl
				myIt <- readSTRef it  
				--traceShowM " try xl:"
				--traceShowM myXl 
				--unsafeIOToST (myTrace ("\n try xl "++show myXl++" and it "++show myIt))

				if(myXl == myIt) then do 
					--traceShowM " d7"
					--unsafeIOToST (myTrace "\n going to d7")
					uncover myIt nodes items
					writeSTRef isD8 True
					writeSTRef isD5 False
				else do 
					pos <- readSTRef l
					writeArray ans pos myXl
					p<- newSTRef (myXl+1)
					whileM (
						do 
							pp<-readSTRef p 
							return (pp/=myXl)
						)(
						do 
							pp<- readSTRef p 
							jj <- readArray nodes pp 
							j <- getAttr itm jj 
							if (j <= 0) then do 
								pup <- getAttr up jj 
								writeSTRef p pup 
							else do 
								cover j nodes items
								modifySTRef p (+1)
						)
					modifySTRef l (+1)
					writeSTRef isD2  True
					writeSTRef isD5 False
					--writeSTRef isD8 False


			else return ()
			d6 <- readSTRef isD6

			if ( d6) then do 
				--traceShowM " d6 ......"
				--unsafeIOToST (myTrace "\n D6.....\n try again")
				--traceShowM" try again" 
				pl <- readSTRef l 
				x <- readArray ans pl 
				writeSTRef xl x 
				p <- newSTRef (x-1)
				whileM (
						do 
							pp<-readSTRef p 
							myXl <- readSTRef xl 
							return (pp/=myXl)
						)(
						do 
							pp<- readSTRef p 
							jj <- readArray nodes pp 
							j <- getAttr itm jj 
							if (j <= 0) then 
								do 
									m <- getAttr down jj 
									writeSTRef p m 
							else do 
								uncover j nodes items
								modifySTRef p (+(-1))
						)
				xxl <- readArray nodes x 
				itxl <- getAttr itm xxl 
				dxxl <- getAttr down xxl
				writeSTRef it itxl 
				writeSTRef xl dxxl
				writeSTRef isD6 False
				writeSTRef isD5 True
				writeSTRef isD8 False

			else return ()

			d8 <- readSTRef isD8

			if(d8) then do 
				--traceShowM "d8....."
				--unsafeIOToST (myTrace "\n D8.....")
				writeSTRef isD8 False
				ll <- readSTRef l 
				--writeSTRef isD8 False
				if(ll==0) then do 
					--traceShowM " finished with "
					c <- readSTRef count
					traceShowM c
					traceShowM " solutions"
					unsafeIOToST (myTrace (" \nfinished with "++show c ++" solutions"))
					writeSTRef status False
				else do 
					modifySTRef l (+(-1))
					writeSTRef isD6 True
					writeSTRef isD2 False
			else return ()

			

		)



main = do 
	stToIO $ algorithmD
	putStrLn" done "

--yoyo ::[[a]]->(STArray s Int (Node s),STArray s Int (Item s))
yoyo arr =  do 
	unsafeIOToST ( myTrace " initialize start")
	--traceShowM " initialize done "
	nds <-  newArray (0,50000) Null::ST s (STArray s Int (Node s))
	its <-  newArray (0,50000) Nill::ST s (STArray s Int (Item s))

	-- skip comments
	p<- newSTRef 0 
	whileM(
		do 
			pp <- readSTRef p 
			return (((arr!!pp)!!0)=="|")
		)(
		modifySTRef p (+1)
		)
	pp<- readSTRef p 
	let x = arr!!pp
	-- copy items and header of nodes
	k<- newSTRef 1
	--first item/node
	n<-newNode 0 0 0
	i<- newItem 0 0 0 "0"
	writeArray nds 0 n 
	writeArray its 0 i 
	forM_(zip [0..(length x -1)] x)  $ \(i,xx)-> do 
		if (xx/="|") then do 
			pos<- readSTRef k 
			ii <- readArray its 0 
			ip <- getAttr' prev ii 
			it<-  newItem ip 0 0 xx
			nd <- newNode  pos pos pos
			inx <- readArray its ip 
			setAttr' next inx pos
			setAttr' prev ii pos
			writeArray nds pos nd
			writeArray its pos it 
			modifySTRef k (+1)

		else return ()
	modifySTRef p (+1)
	--modifySTRef k (+1)
	pp<- readSTRef p
	--traceShowM pp 
	let (rest,arrr) = splitAt pp arr 
	pos<- readSTRef k
	spT <- newSTRef 0 
	spU <- newSTRef pos
	forM_ (zip[0..((length arrr ))] arrr) $ \(i,xx) -> do 
		sT <- readSTRef spT
		sU <- readSTRef spU
		pos<- readSTRef k
		let ssU = pos- sU
		spa <- newNode ssU 0  sT 
		writeSTRef spU 0 
		writeArray nds pos spa 
		modifySTRef k (+1)


		forM_(zip [0..(length xx -1)] xx)$ \(j,jj) -> do 
			
				indx <- findPosition jj its
				--traceShowM jj 
				--traceShowM indx
				a<-newSTRef 0
				inu <- readArray nds indx
				indU <- getAttr up inu
				--inD <- getAttr down inu 
				--traceShowM inD
				pos<- readSTRef k 
				tnd <- newNode indU indx indx
				setAttr up inu pos
				nd <- readArray nds indU
				setAttr down nd pos
				writeArray nds pos tnd

				ci <- readArray its indx
				cin <- getAttr' numNodes ci 
				setAttr' numNodes ci (cin+1)
				modifySTRef spU (+1)
				modifySTRef k (+1)
		pos <-readSTRef k

		setAttr down spa (pos-1) 
		modifySTRef spT (+(-1))
	--last spacer
	pos<- readSTRef k
	sT <- readSTRef spT
	sU <- readSTRef spU
	lsp <- newNode (pos-sU) (-99999) sT
	writeArray nds pos lsp  
	--ae <- readNodes nds 
	--unsafeIOToST (myTrace (show ae)) 
	--unsafeIOToST(myTrace " initialize end")
	--traceShowM " initialize done....."


	return (nds,its)

testYoyo = do 
	(nds,its)<-stToIO$ yoyo [["a","b","c","d","e","f","g"],["c","e"],["a","d","g"],["b","c","f"],["a","d","f"],["b","g"],["d","e","g"]]
	nList<-stToIO $ readNodes nds
	--iList <- stToIO$ readItems its 
	putStrLn ("All nodes = ")-- ++ show nList)
	putStrLn ("All items = ")-- ++ show iList)

printAns :: STArray s Int Int->ST s [Int]
printAns nodes = do 
	(lower, upper) <- getBounds nodes
	nods <- mapM (\i -> readArray nodes i) [lower..upper-1]
	xx <- mapM ( \i -> f(nods !! i)) [0..upper-lower-1]
	return xx
	where f a = do 
		traceShowM a 
		return a
	   	    

myTrace :: String-> IO ()
myTrace ss  = do 
	let file = "C:\\Users\\epan0\\OneDrive\\Back Arbeit\\Dancing Links\\dlx1\\Output.txt"
	appendFile file ss


{--
another = do 
	traceShowM " input file "
	i <- newSTRef 0 
	let list= mains 
	--skip first comments
	p <- newSTRef 0 
	whileM (do 
		pp<- readSTRef p 
			--traceShowM pp 
		let x = words (list !! pp )
			--traceShowM (x !!0 )
		return ((x!!0)=="|")
		)(do 
			modifySTRef p (+1)
		)
	writeSTRef i 2 
	--print l 

mains = do  
        let list = []
        handle <- openFile "C:\\Users\\epan0\\Desktop\\test.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = lines contents
        return singlewords  	




--trying::[[Int]]->IO () "C:\\Users\\epan0\\Desktop\\test.txt"

trying = do 
	putStrLn " input the file addres: "
	file <- getLine 
	handle <- openFile file ReadMode

	contents <- hGetContents handle
	let list = lines contents
	forM_(zip [0..(length list -1)] list)  $ \(i,xx)-> do 
		print xx
		forM_ ( zip [0..(length xx -1)] xx) $ \(j,k)-> do 
			


	hClose handle
--}


f 1 =1
f n = n*f(n-1)