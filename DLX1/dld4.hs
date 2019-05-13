import Data.Array.Unboxed (UArray, (!))
import Control.Monad.ST.Lazy
import Control.Monad (replicateM, forM_, when)
import Data.List
import Debug.Trace(trace,traceShowM)
import Data.STRef.Lazy
import Data.Array.ST (STArray, newArray, readArray, writeArray,getBounds)
import Data.Array.IArray (listArray)
import System.IO
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad.Loops (whileM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.ST.Lazy.Unsafe (unsafeIOToST)


data Node s = Node { up, down,itm :: STRef s  Int} |Null 

data Item  s = Item { prev, next,numNodes :: STRef s Int,
                     name ::STRef s String } |Nill 

instance Eq (Node s) where
	a==b = (up a == up b) && (down a == down b)&&(itm a == itm b)



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

dld ::STRef s Int->STRef s Int->STRef s Int-> STArray s Int (Node s)->STArray s Int (Item s) ->STRef s Int ->STArray s Int Int->ST s ()
dld count it xl nodes items l ans= do 
	
	d2  
	where

	d2 = do 
				rrlink <- readArray items 0 
				rlink <- getAttr' next rrlink
				if(rlink==0) then do 
					modifySTRef count (+1)
					d8 
				else do 
					i<-choseIt items 
					writeSTRef it i
					ir <- readArray nodes i 
					ii <- getAttr down ir 
					writeSTRef xl ii
					cover i
					d5 


	d5 = do 
				xxl <- readSTRef xl 
				iit <- readSTRef it 
				if ( xxl == iit) then do 
					uncover iit 
					d8 
				else do 
					pos <- readSTRef l
					myXl <- readSTRef xl 
					writeArray ans pos myXl
					d55  myXl (myXl+1) 
					modifySTRef l (+1)
					d2 


	d55 xl xxl  = do 
		if (xl /= xxl ) then do 
								jj <- readArray nodes xxl 
								j <- getAttr itm jj 
								if (j <= 0) then do 
									pup <- getAttr up jj 
									d55 xl pup 
								else do 
									cover j 
									d55 xl (xxl+1) 
		else return ()
							
	d6 = do 
					pl <- readSTRef l 
					x <- readArray ans pl 
					writeSTRef xl x 
					d66 x (x-1) 
					
					xxl <- readArray nodes x 
					itxl <- getAttr itm xxl 
					dxxl <- getAttr down xxl
					writeSTRef it itxl 
					writeSTRef xl dxxl
					d5 

	d66 xl xxl  = do 
		if (xl/=xxl) then do 
								jj <- readArray nodes xxl 
								j <- getAttr itm jj 
								if (j <= 0) then do 
										m <- getAttr down jj 
										d66 xl m 
								else do 
									uncover j 
									d66 xl (xxl -1) 
		else return ()


	d8 = do 
				ll<- readSTRef l
				if (ll == 0) then do 
					traceShowM "FINISHED  "
					c<- readSTRef count
					unsafeIOToST (myTrace ("\nsolution found: "++show c))
				else do
					ll<- readSTRef l
					writeSTRef l (ll-1)
					d6 

	 


	cover it    = 
		do 
			currNode <-  readArray nodes it
			nd <- getAttr down currNode
			coverLoop nd 
			currIt<- readArray items it
			l<-getAttr' prev currIt
			r<-getAttr' next currIt

			lItem <- readArray items l
			rItem <- readArray items r

			setAttr' next lItem  r
			setAttr' prev rItem l

			where 
				coverLoop  iit  = do 
					if(it/=iit) then do 
							  hide (iit+1) 
							  nxNode<-readArray nodes iit
							  xy<- getAttr down nxNode
							  coverLoop  xy 
					else return ()

					where 
						hide q  = do 
							if (iit/=q) then do 
										xx<- readArray nodes q
										x <- getAttr itm xx
										u <- getAttr up xx

										if(x<=0) then hide u  
										else 
										    do 
										     uu <- readArray nodes u 
										     d <- getAttr down xx
										     dd <- readArray nodes d 

										     setAttr down uu d  
										     setAttr up dd u
										     cc <- readArray items x 
										     c <- getAttr' numNodes cc
										     setAttr' numNodes cc (c-1)
										     hide (q+1) 
							else return [()] 
							
	uncover it   = 
		do 
			currNode <-  readArray nodes it
			nd <- getAttr up currNode
			uncoverLoop nd 
			currIt<- readArray items it

			l<-getAttr' prev currIt
			r<-getAttr' next currIt

			lItem <- readArray items l
			rItem <- readArray items r

			setAttr' next lItem  it
			setAttr' prev rItem it

		where

		uncoverLoop iit = do 
			if(it/=iit) then do 
					  unhide (iit-1) 
					  nxNode<-readArray nodes iit
					  xy<- getAttr up nxNode
					  uncoverLoop xy 
			else return ()
			
			where

			unhide q  = do 
				if (iit/=q) then do 
							xx<- readArray nodes q
							x <- getAttr itm xx
							d <- getAttr down xx

							if(x<=0) then unhide d 
							else 
							    do 
							     u <- getAttr up xx 
							     uu <- readArray nodes u 
							     dd <- readArray nodes d 

							     setAttr down uu q  
							     setAttr up dd q
							     cc <- readArray items x 
							     c <- getAttr' numNodes cc
							     setAttr' numNodes cc (c+1)
							     unhide (q-1) 
				else return [()]

				

printAnswer:: STArray s Int Int-> STArray s Int (Node s)->STArray s Int (Item s) ->ST s[[Char]]
printAnswer ans nodes items = do 
	(l,u) <- getBounds ans 
	xx <- mapM( \i -> f i) [0..(u-l-1)]
	return xx
	   where 
	   	    f  y= 	   		
	   	      do
	   	      	x <- readArray ans y
	   	      	if (x/=0) then do 
		   	        nd  <- readArray nodes x
		   	        ni <- getAttr itm nd 
		   	        nii <- readArray items ni 
		   	        niin <- getAttr' name nii 
		   	        p<- newSTRef (x+1)
		   	        s <- newSTRef (" "++show niin)
		   	        whileM (do 
		   	        	pos <- readSTRef p 
		   	        	return (pos/=x)) (
		   	        	do 
		   	        		pos <- readSTRef p
		   	        		nx <- readArray nodes pos
		   	        		nit <- getAttr itm nx 
		   	        		if(nit<0) then do 
		   	        			itp <- getAttr up nx 
		   	        			writeSTRef p itp
		   	        		else do 
		   	        			nxi <- readArray items nit 
		   	        			nxn <- getAttr' name nxi 
		   	        			modifySTRef s (++ (" - " ++(show nxn)))
		   	        			modifySTRef p (+1)

		   	        	)
		   	        ss <- readSTRef s 
		   	         
	   	        	return ss
	   	        else return []


startAlgorithmD = do
	let y = unsafePerformIO readFromFile
	(nodes,items)<- initialize y 
	l <- newSTRef 0 
	it <- newSTRef 0
	xl <- newSTRef 0
	c <- newSTRef 0
	ans <-newArray (0,20) 0 :: ST s (STArray s Int Int)

	m <-dld c it xl nodes items l ans 
	traceShowM "--------"
		

main = do stToIO$ startAlgorithmD

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

initialize arr = do 
	unsafeIOToST ( myTrace " initialize start")
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
	unsafeIOToST(myTrace (" initialize finish with "++ show pos ++ " nodes"))


	return (nds,its)
