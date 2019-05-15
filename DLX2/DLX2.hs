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


import Data.Char (ord)


data Node s = Node { up, down,color,itm :: STRef s  Int} |Null 

data Item  s = Item { prev, next,numNodes :: STRef s Int,
                     name ::STRef s String } |Nill 

instance Eq (Node s) where
	a==b = (up a == up b) && (down a == down b)&&(itm a == itm b)


nods = newArray (0,100000) Null :: ST s (STArray s Int (Node s))
itms = newArray (0,1000) Nill :: ST s (STArray s Int (Item s))

addNode :: Int->Int ->Int ->Int ->STArray s Int (Node s)->Int->ST s ()
addNode u d c i arr pos = do 
	nd <- newNode u d c i 
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



readNodes :: STArray s Int (Node s )->ST s [(Int,Int,Int,Int)]
readNodes nodes = do 
	(lower, upper) <- getBounds nodes
	nods <- mapM (\i -> readArray nodes i) [lower..upper-1]
	xx <- mapM ( \i -> f (nods !! i)) [0..upper-lower-1]
	return xx
	   where 
	   	    f Null = return (0, 0,0,0)
	   	    f (Node {up=u, down=d,color=c, itm = i}) = 	   		
	   	      do
	   	        u <- readSTRef u 
	   	        d <- readSTRef d 
	   	        c <- readSTRef c
	   	        i <- readSTRef i  
	   	         
	   	        return (u,d,c,i) 	    

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


newNode u d c i =
        do 
		 uu <- newSTRef u
		 dd <- newSTRef d
		 cc <- newSTRef c
		 ii <- newSTRef i
		 return ( Node uu dd cc ii )


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



hide:: Int->STArray s Int (Node s)->STArray s Int (Item s) ->ST s ()
hide p nods cols  = 
	do 
		currNode<- readArray nods p 
		q<- newSTRef (p+1) 
		--traceShowM "hide"
		--traceShowM p
		whileM_ ( do 
			q'<- readSTRef q
			--traceShowM q'
			return(q'/=p)) (
			do 
				q'<-readSTRef q 
				xx<- readArray nods q'
				c<- getAttr color xx
				if(c>=0) then do 
					x <- getAttr itm xx

					u <- getAttr up xx

					d <- getAttr down xx

					if(x<=0) then writeSTRef q (u-1) 
					else 
					    do 
					     uu <- readArray nods u 
					     dd <- readArray nods d 

					     setAttr down uu d  
					     setAttr up dd u
					     cc <- readArray cols x 
					     c <- getAttr' numNodes cc
					     setAttr' numNodes cc (c-1)
				else return()
 

				modifySTRef q (+1)
				    -- z <- readSTRef q 
				     --traceShowM z	


			)




cover :: Int->STArray s Int (Node s)->STArray s Int (Item s) ->ST s ()
cover it nods cols  = 
	do 
		--unsafeIOToST (myTrace ("\ncover item "++show it))
		currNode <-  readArray nods it
		nd <- getAttr down currNode
		pos <- newSTRef nd 
		--traceShowM (" covering item "++show it )
		
		whileM_( do 
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
		


unhide:: Int->STArray s Int (Node s)->STArray s Int (Item s) ->ST s ()
unhide p nods cols  = 
	do 
		currNode<- readArray nods p 
		q<- newSTRef (p-1) 
		--traceShowM p
		--traceShowM "unhide"
		whileM_ ( do 
			q'<- readSTRef q
			--traceShowM q'
			return(q'/=p)) (
			do 
				q'<-readSTRef q 
				xx<- readArray nods q'
				c<- getAttr color xx 
				if(c>=0) then do 
					x <- getAttr itm xx

					u <- getAttr up xx

					d <- getAttr down xx

					if(x<=0) then writeSTRef q (d+1) 
					else 
					    do 
					     uu <- readArray nods u 
					     dd <- readArray nods d 

					     setAttr down uu q'  
					     setAttr up dd q'
					     cc <- readArray cols x 
					     c <- getAttr' numNodes cc
					     setAttr' numNodes cc (c+1)
				else return ()
 

				modifySTRef q (+(-1))
				    -- z <- readSTRef q 
				     --traceShowM z	


			)


uncover :: Int->STArray s Int (Node s)->STArray s Int (Item s) ->ST s ()
uncover it nods cols  = 
	do 
		--unsafeIOToST (myTrace ("\n uncover item "++show it))
		currNode <-  readArray nods it
		nd <- getAttr up currNode
		pos <- newSTRef nd 
		--traceShowM (" uncovering item "++show it )
		
		whileM_( do 
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

purify::STArray s Int (Node s )->STArray s Int (Item s )->Int->ST s ()
purify nods its p= do 
	--traceShowM ("purify "++show p)
	nd <- readArray nods p
	c <- getAttr color nd 
	i <- getAttr itm nd 
	ii <-readArray nods i
	nn <- getAttr down ii 
	q <- newSTRef nn
	--traceShowM ii 


	whileM_ (do 
		qq<- readSTRef q 
		--traceShowM qq
		return (qq/=i)
		)(
		do 
			qq<- readSTRef q 
			d <- readArray nods qq 
			dd <- getAttr down d 
			dc <- getAttr color d 
			--traceShowM "color"
			
			--traceShowM " pp"
			--traceShowM qq
			if (dc/=c) then do 
				--traceShowM "ahaha"
				hide qq nods its 
				--traceShowM qq 
				--traceShowM dc
			else if (qq/=p) then do  
			  setAttr color d (-1)
			  --traceShowM "koko"
			  --traceShowM qq
			  return ()
			  else return ()
			--else return [()]
			writeSTRef q dd 

		)
	return ()

unpurify::STArray s Int (Node s )->STArray s Int (Item s )->Int->ST s ()
unpurify nods its p= do 
	--traceShowM ("unpurify "++show p)
	nd <- readArray nods p
	c <- getAttr color nd 
	i <- getAttr itm nd 
	ii <-readArray nods i
	nn <- getAttr down ii 
	q <- newSTRef nn
	--traceShowM ii 


	whileM_ (do 
		qq<- readSTRef q 
		--traceShowM qq
		return (qq/=i)
		)(
		do 
			qq<- readSTRef q 
			d <- readArray nods qq 
			dd <- getAttr down d 
			dc <- getAttr color d 
			--traceShowM "color"
			
			--traceShowM " pp"
			--traceShowM qq
			if (dc<0) then do 
				--traceShowM "ahaha"
				setAttr color d c
				--unhide qq nods its 
				--traceShowM qq 
				--traceShowM dc
			else if (qq/=p) then do 
			  --setAttr color d (-1)
			  unhide qq nods its
			  --traceShowM "koko"
			  --traceShowM qq
			  else return ()
			writeSTRef q dd 

		)

	return ()


--yoyo ::[[a]]->(STArray s Int (Node s),STArray s Int (Item s))
initialize arr = do 
	nds <-  newArray (0,20000) Null::ST s (STArray s Int (Node s))
	its <-  newArray (0,1000) Nill::ST s (STArray s Int (Item s))
	sec <- newSTRef 10000 

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
	n<-newNode 0 0 0 0
	i<- newItem 0 0 0 "0"
	writeArray nds 0 n 
	writeArray its 0 i 
	forM_(zip [0..(length x -1)] x)  $ \(i,xx)-> do 
		pos <- readSTRef k 
		if (xx/="|") then do
			ii <- readArray its 0 
			ip <- getAttr' prev ii 
			it<-  newItem ip 0 0 xx
			nd <- newNode  pos pos 0 pos
			inx <- readArray its ip 
			setAttr' next inx pos
			setAttr' prev ii pos
			writeArray nds pos nd
			writeArray its pos it 
			modifySTRef k (+1)

		else do 
			writeSTRef sec pos 

	pos <- readSTRef k
	second <- readSTRef sec
	traceShowM ("second ....."++show second)
	if(second >=pos) then writeSTRef sec pos
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
		spa <- newNode ssU 0 0 sT 
		writeSTRef spU 0 
		writeArray nds pos spa 
		modifySTRef k (+1)


		forM_(zip [0..(length xx -1)] xx)$ \(j,jj) -> do 
				jjj<- newSTRef jj
				cc <- newSTRef 0
				if (contains jj ':') then do 
					let len = length jj 
					let (a,b) = splitAt (len -2) jj 
					let (_,c) = splitAt 1 b
					writeSTRef jjj a 
					let c0 = c !!0
					writeSTRef cc (ord c0) 
				else return ()

				nc <- readSTRef cc
				nj <- readSTRef jjj 
				indx <- findPosition nj its
				--traceShowM jj 
				--traceShowM indx
				a<-newSTRef 0
				inu <- readArray nds indx
				indU <- getAttr up inu
				--inD <- getAttr down inu 
				pos<- readSTRef k 
				tnd <- newNode indU indx nc indx
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
	lsp <- newNode (pos-sU) (-99999) 0 sT
	writeArray nds pos lsp  
	second <- readSTRef sec
	n0<- readArray its 0 
	setAttr' prev n0 (second-1) 
	ns <- readArray its (second-1)
	setAttr' next ns 0 


	return (nds,its,second)



contains [] a = False
contains (b:bs) a= if(a==b) then True else contains bs a



readFromFile = do 
	putStrLn " input the file addres: "
	file <- getLine 
	handle <- openFile file ReadMode

	contents <- hGetContents handle
	let list =lines contents
	let  y=length list
	let alla = map(\i-> words (list!!i))[0..y-1]
	--hClose handle

	return alla 


myTrace :: String-> IO ()
myTrace ss  = do 
	let file = "C:\\Users\\epan0\\OneDrive\\Back Arbeit\\Dancing Links\\dlx2\\Output.txt"
	appendFile file ss


main = do 
	stToIO $ initAlgo
	putStrLn" done "


initAlgo = do
	let y = unsafePerformIO readFromFile
	(nodes,items,second)<- initialize y 

	l <- newSTRef 0 
	it <- newSTRef 0
	xl <- newSTRef 0
	c <- newSTRef 0
	ans <-newArray (0,20) 0 :: ST s (STArray s Int Int)
	

	m <-dlc c it xl nodes items l ans
	traceShowM "--------"


dlc ::STRef s Int->STRef s Int->STRef s Int-> STArray s Int (Node s)->STArray s Int (Item s) ->STRef s Int ->STArray s Int Int->ST s ()
dlc count it xl nodes items l ans  = do 
	
	c2 
	where
		c2 = do 
			--unsafeIOToST (myTrace "\nD2.......")
			rrlink <- readArray items 0 
			rlink <- getAttr' next rrlink
			if(rlink==0) then do 
				--traceShowM " solution found"
				--unsafeIOToST (myTrace ("\nsolution found"))

				--printAns ans
				modifySTRef count (+1)
				c <- readSTRef count
				traceShowM c
				c8
			else c3 
		c3 = do 
			--traceShowM c
			--unsafeIOToST (myTrace ("\nd3......."))
			i<-choseIt items 
			--let c = "chose i"++i++"hh"
			--unsafeIOToST (myTrace ("\nchosen it "++show i))
			writeSTRef it i
			ir <- readArray nodes i 
			ii <- getAttr down ir 
			writeSTRef xl ii
			c4
		c4 = do 
			--unsafeIOToST (myTrace ("\nd4....."))
			i <- readSTRef it
			cover i nodes items
			c5 


		c5 = do 
			--traceShowM "try xl "
			--unsafeIOToST (myTrace ("\nd5......."))
			xxl <- readSTRef xl 
			iit <- readSTRef it 
			if ( xxl == iit) then do 
				--traceShowM "xl eq it goto d7"
				--unsafeIOToST (myTrace ("\nxl equal to it\n go to d5"))
				c7
			else do 
				pos <- readSTRef l
				--myXl <- readSTRef xl 
				writeArray ans pos xxl
				p<- newSTRef (xxl+1)
				--traceShowM myXl
				--unsafeIOToST (myTrace ("\nmy xl: "++show myXl))
				whileM (
						do 
							pp<-readSTRef p 
							--traceShowM pp
							return (pp/=xxl)
						)(
						do 
							pp<- readSTRef p 
							jj <- readArray nodes pp 
							j <- getAttr itm jj 
							if (j <= 0) then do 
								pup <- getAttr up jj 
								writeSTRef p pup
							else do 
								c<- getAttr color jj 
								if (c ==0) then do 
									cover j nodes items
								else if (c>0) then purify nodes items pp
									 else return ()

								modifySTRef p (+1)
						)
				modifySTRef l (+1)
				c2


		c6 = do 
				--traceShowM " try again"
				--unsafeIOToST (myTrace ("\n D6......"))
				pl <- readSTRef l 
				x <- readArray ans pl 
				writeSTRef xl x 
				p <- newSTRef (x-1)
				myXl <- readSTRef xl 
				whileM (
						do 
							pp<-readSTRef p 
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
								c<- getAttr color jj 
								if (c==0) then do 
									uncover j nodes items
								else if (c >0) then unpurify nodes items pp 
									 else return ()
								modifySTRef p (+(-1))
						)

				xxl <- readArray nodes x 
				itxl <- getAttr itm xxl 
				dxxl <- getAttr down xxl
				writeSTRef it itxl 
				writeSTRef xl dxxl
				c5

		c7 = do 
			iit <- readSTRef it 
			--traceShowM " uncover"
			--traceShowM iit 
			uncover iit nodes items
			c8 

		c8 = do 
			--traceShowM " d8....."
			--unsafeIOToST (myTrace (" \n d8....."))
			ll<- readSTRef l
			if (ll == 0) then do 
				traceShowM "FINISHED  "
				c<- readSTRef count
				--traceShowM " solutions"
				unsafeIOToST (myTrace ("\n\nsolution found: "++show c))
			else do
				ll<- readSTRef l
				--traceShowM " d6?"
				writeSTRef l (ll-1)
				c6


