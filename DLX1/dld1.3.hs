import Control.Monad.ST.Lazy
import Control.Monad (replicateM, forM_, when)
import Data.List
import Debug.Trace(trace,traceShowM)
import Data.STRef.Lazy 
import Data.Array.ST (STArray, newArray, readArray, writeArray,getBounds)
import Data.Array.IArray (listArray)
import System.IO
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad.Loops (whileM,whileM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.ST.Lazy.Unsafe (unsafeIOToST)
{--
--import Control.Exception

--import Debug.Trace(trace,traceShowM)
import Formatting
import Formatting.Clock
import System.Clock
--}
{-# INLINE hide #-}
{-# INLINE unhide #-}
{-# INLINE cover #-}
{-# INLINE uncover #-}
{-# INLINE uncoverLoop #-}
{-# INLINE coverLoop #-}
{-# INLINE d55 #-}
{-# INLINE d66 #-}
{-# INLINE choseIt#-}





data Node s = Node { up, down,itm :: STRef s  Int} |Null 

data Item  s = Item { prev, next,numNodes :: STRef s Int,
                     name ::STRef s String } |Nill 

instance Eq (Node s) where
	a==b = (up a == up b) && (down a == down b)&&(itm a == itm b)



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


findPosition :: String -> STArray s Int (Item s )->ST s Int
findPosition nm cols = do 
	curi <- readArray cols 0
	--traceShowM nm
	p <- getAttr' next curi
	--traceShowM p
	pos <- newSTRef p 
	whileM_(
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




hide:: Int->Int->STArray s Int (Node s)->STArray s Int (Item s) ->ST s [()]
hide p q nods cols = do 
	if (p/=q) then do 
				--traceShowM ("unhide"++show q)
				xx<- readArray nods q
				x <- getAttr itm xx
				u <- getAttr up xx
				d <- getAttr down xx

				if(x<=0) then hide p u nods cols 
				else 
				    do 
				     uu <- readArray nods u 
				     dd <- readArray nods d 

				     setAttr down uu d  
				     setAttr up dd u
				     cc <- readArray cols x 
				     c <- getAttr' numNodes cc
				     setAttr' numNodes cc (c-1)
				     hide p (q+1) nods cols
	else return [()]



cover :: Int->STArray s Int (Node s)->STArray s Int (Item s) ->ST s ()
cover it nods cols  = 
	do 
		--traceShowM ("uncover item "++show it  )
		--uncover can also be eliminated -- paste everything in else and 
		--unsafeIOToST (myTrace ("\nuncover item "++show it))
		--s<-unsafeIOToST getSTime
		currNode <-  readArray nods it
		nd <- getAttr down currNode
		--traceShowM it
		coverLoop it nd nods cols
		currIt<- readArray cols it
		l<-getAttr' prev currIt
		r<-getAttr' next currIt

		lItem <- readArray cols l
		rItem <- readArray cols r

		setAttr' next lItem  r
		setAttr' prev rItem l

		--e<-unsafeIOToST getEtime
		--let z = exTime s e 

		--end <-unsafeIOToST getCurrentTime
		--unsafeIOToST (myTrace ("\nuncover time  "++show z))


coverLoop it iit nodes items = do 
	if(it/=iit) then do 
			  --traceShowM ("unhide"++ show iit)
			  hide iit (iit+1) nodes items
			  nxNode<-readArray nodes iit
			  xy<- getAttr down nxNode
			  coverLoop it xy nodes items
	else return ()
		




unhide:: Int->Int->STArray s Int (Node s)->STArray s Int (Item s) ->ST s [()]
unhide p q nods cols = do 
	if (p/=q) then do 
				--traceShowM ("unhide"++show q)
				xx<- readArray nods q
				x <- getAttr itm xx
				u <- getAttr up xx
				d <- getAttr down xx

				if(x<=0) then unhide p d nods cols 
				else 
				    do 
				     uu <- readArray nods u 
				     dd <- readArray nods d 

				     setAttr down uu q  
				     setAttr up dd q
				     cc <- readArray cols x 
				     c <- getAttr' numNodes cc
				     setAttr' numNodes cc (c+1)
				     unhide p (q-1) nods cols
	else return [()]
 



uncover :: Int->STArray s Int (Node s)->STArray s Int (Item s) ->ST s ()
uncover it nods cols  = 
	do 
		--traceShowM ("uncover item "++show it  )
		--uncover can also be eliminated -- paste everything in else and 
		--unsafeIOToST (myTrace ("\nuncover item "++show it))
		--s<-unsafeIOToST getSTime
		currNode <-  readArray nods it
		nd <- getAttr up currNode
		--traceShowM it
		uncoverLoop it nd nods cols
		currIt<- readArray cols it
		l<-getAttr' prev currIt
		r<-getAttr' next currIt

		lItem <- readArray cols l
		rItem <- readArray cols r

		setAttr' next lItem  it
		setAttr' prev rItem it

		--e<-unsafeIOToST getEtime
		--let z = exTime s e 

		--end <-unsafeIOToST getCurrentTime
		--unsafeIOToST (myTrace ("\nuncover time  "++show z))


uncoverLoop it iit nodes items = do 
	if(it/=iit) then do 
			  --traceShowM ("unhide"++ show iit)
			  unhide iit (iit-1) nodes items
			  --unhide iit nodes items
			  nxNode<-readArray nodes iit
			  xy<- getAttr up nxNode
			  uncoverLoop it xy nodes items
	else return ()


choseIt :: STArray s Int (Item s)->ST s (Int)
choseIt cols = 
	do 
		ii<- readArray cols 0 
		i<- getAttr' next ii 

		m<- newSTRef 100000000
		curi  <- newSTRef i
		best <-  newSTRef 100 
		bestV <- newSTRef 100

		whileM_ (
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
				--unsafeIOToST (myTrace ("\ncurrent it "++show x ++" current nn "++show currNn))
				if (currNn < v) then 
				   do 
				     writeSTRef best x
				     writeSTRef bestV currNn
				     --unsafeIOToST (myTrace ("\nmy best is now "++show x))
				     --traceShowM "pass"
				else return ()
				writeSTRef curi nx
				--traceShowM x

				)
		bst <- readSTRef best
		--traceShowM " chosen item is "
		--traceShowM bst
		return  bst


trying = do 
	putStrLn " input the file addres: "
	file <- getLine 
	handle <- openFile file ReadMode

	contents <- hGetContents handle
	let list =lines contents
	let  y=length list
	let alla = map(\i-> words (list!!i))[0..y-1]
	--hClose handle

	return alla 



initialize arr = do 
	unsafeIOToST ( myTrace " initialize start")
	nds <-  newArray (0,50000) Null::ST s (STArray s Int (Node s))
	its <-  newArray (0,50000) Nill::ST s (STArray s Int (Item s))

	-- skip comments
	p<- newSTRef 0 
	whileM_(
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
	unsafeIOToST(myTrace (" initialize finish with "++ show pos ++ " nodes"))


	return (nds,its)


dld ::STRef s Int->STRef s Int->STRef s Int-> STArray s Int (Node s)->STArray s Int (Item s) ->STRef s Int ->STArray s Int Int->ST s ()
dld count it xl nodes items l ans= do 
	
	--(nodes,items)<- yoyo [["a","b","c","d","e","f","g"],["c","e"],["a","d","g"],["b","c","f"],["a","d","f"],["b","g"],["d","e","g"]] 
	d2 count it xl nodes items l ans

d2::STRef s Int->STRef s Int->STRef s Int-> STArray s Int (Node s)->STArray s Int (Item s) ->STRef s Int ->STArray s Int Int->ST s ()
d2 count it xl nodes items l ans= do 
			--unsafeIOToST (myTrace "\nD2.......")
			rrlink <- readArray items 0 
			rlink <- getAttr' next rrlink
			if(rlink==0) then do 
				--traceShowM " solution found"
				--unsafeIOToST (myTrace ("\nsolution found"))

				--printAns ans
				modifySTRef count (+1)
				--c <- readSTRef count
				--traceShowM c
				d8 count it xl nodes items l ans
			else d3 count it xl nodes items l ans

d3 count it xl nodes items l ans= do 
			--traceShowM c
			--unsafeIOToST (myTrace ("\nd3......."))
			i<-choseIt items 
			--let c = "chose i"++i++"hh"
			--unsafeIOToST (myTrace ("\nchosen it "++show i))
			writeSTRef it i
			ir <- readArray nodes i 
			ii <- getAttr down ir 
			writeSTRef xl ii
			d4 count it xl nodes items l ans

d4 count it xl nodes items l ans = do 
			--unsafeIOToST (myTrace ("\nd4....."))
			i <- readSTRef it
			cover i nodes items
			d5 count it xl nodes items l ans


d5 count it xl nodes items l ans= do 
			--unsafeIOToST (myTrace ("\nd5......."))
			xxl <- readSTRef xl 
			iit <- readSTRef it 
			if ( xxl == iit) then do 
				--traceShowM "xl eq it goto d7"
				--unsafeIOToST (myTrace ("\nxl equal to it\n go to d5"))
				d7 count it xl nodes items l ans
			else do 
				pos <- readSTRef l
				myXl <- readSTRef xl 
				writeArray ans pos myXl
				--p<- newSTRef (myXl+1)
				--traceShowM myXl
				--unsafeIOToST (myTrace ("\nmy xl: "++show myXl))
				d55  myXl (myXl+1) nodes items
				modifySTRef l (+1)
				d2 count it xl nodes items l ans


d55 xl xxl nodes items = do 
	if (xl /= xxl ) then do 
							jj <- readArray nodes xxl 
							j <- getAttr itm jj 
							if (j <= 0) then do 
								pup <- getAttr up jj 
								d55 xl pup nodes items
							else do 
								cover j nodes items
								d55 xl (xxl+1) nodes items
	else return ()
						
d6 count it xl nodes items l ans= do 
				--traceShowM " try again"
				--unsafeIOToST (myTrace ("\n D6......"))
				pl <- readSTRef l 
				x <- readArray ans pl 
				writeSTRef xl x 
				p <- newSTRef (x-1)
				myXl <- readSTRef xl 

				d66 myXl (myXl-1) nodes items
				
				xxl <- readArray nodes x 
				itxl <- getAttr itm xxl 
				dxxl <- getAttr down xxl
				writeSTRef it itxl 
				writeSTRef xl dxxl
				d5 count it xl nodes items l ans

d66 xl xxl nodes items = do 
	if (xl/=xxl) then do 
							jj <- readArray nodes xxl 
							j <- getAttr itm jj 
							if (j <= 0) then 
								do 
									m <- getAttr down jj 
									d66 xl m nodes items 
							else do 
								--jjj <- readArray nodes j 
								--jup <- getAttr up jjj
								uncover j nodes items
								d66 xl (xxl -1) nodes items
	else return ()

d7 count it xl nodes items l ans= do 
			iit <- readSTRef it 
			--traceShowM " uncover"
			--traceShowM iit
			--int <- readArray nodes iit 
			--inxt <- getAttr up int  
			uncover iit nodes items
			d8 count it xl nodes items l ans

d8 count it xl nodes items l ans= do 
			--traceShowM " d8....."
			--unsafeIOToST (myTrace (" \n d8....."))
			ll<- readSTRef l
			if (ll == 0) then do 
				traceShowM "FINISHED  "
				c<- readSTRef count
				--traceShowM " solutions"
				unsafeIOToST (myTrace ("\nsolution found: "++show c))
			else do
				ll<- readSTRef l
				--traceShowM " d6?"
				writeSTRef l (ll-1)
				d6 count it xl nodes items l ans



algorithmD = do
	let y = unsafePerformIO trying
	--s <- unsafeIOToST getSTime
	(nodes,items)<- initialize y 
	--unsafeIOToST (myTrace (show y))
	--(nodes,items)<- initialize --yoyo [["a","b","c","d","e","f","g"],["c","e"],["a","d","g"],["b","c","f"],["a","d","f"],["b","g"],["d","e","g"]] 
	l <- newSTRef 0 
	--let y = unsafePerformIO trying
	it <- newSTRef 0
	xl <- newSTRef 0
	c <- newSTRef 0
	ans <-newArray (0,20) 0 :: ST s (STArray s Int Int)

	m <-dld c it xl nodes items l ans 
	--e <-unsafeIOToST getEtime
	--let z = exTime s e 
	--unsafeIOToST (myTrace ("excecution time is "++ show z))
	traceShowM "--------"
	

	
	

main = do stToIO$ algorithmD

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

mains2 =
  do start <- getTime Monotonic
     evaluate (sum [1 .. 1000000])
     end <- getTime Monotonic
     fprint (timeSpecs % "\n") start end

getSTime = do 
	s <- getTime Monotonic
	return s

getEtime = do 
	e <- getTime Monotonic
	return e 

exTime start end= formatToString  (timeSpecs % "\n") start end 

rutwe = do 
	x<- getSTime
	putStrLn " help"
	putStrLn " rutwe"

	y <- getEtime
	let z= exTime x y 
	putStrLn (""++show z ) 
--}


