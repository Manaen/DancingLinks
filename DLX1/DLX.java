package dlx1;
import java.io.*;
//import  java.util.concurrent.TimeUnit;

@SuppressWarnings("Duplicates")
public class DLX {

    private static Item[] column;
    private static Node[] nodes;
    private static int [] answer;
    private static int l;
    private  static File file;
    private static int xl;
    private static int it;
    private static int count;
    //private static final int Maxlevel = 1000;
    private static final  int MaxCol =1000;
    private static final int MaxOpt = 1000;
    private static final int MaxNodes = MaxOpt*MaxCol+MaxOpt;
    //private static int last_item;

    /**
     * finds the position of the element in the items list
     *
     * @param  s the string that needs to be found in the item list
     * @return int pos the index of the string in the item list
     */
    static  int findPosition(String s){
        //System.out.println(s);
        int pos =0;
        int currNod = column[0].next;
        while (currNod!=0){
            if(s.equals(column[currNod].name)) {
                // System.out.println(s);
                pos=currNod;
                break;
            }
            currNod=column[currNod].next;
        }
        if (pos==0){
            System.out.println("err.."+s); System.exit(404);}

        return pos;
    }

    /**
     * D1: Initialize
     */
    static void initialize(){
        System.out.println("initialization .....START");
        column = new Item[MaxCol];
        nodes = new Node[MaxNodes];
        answer= new int[MaxOpt];
        l=0;// keeps the number of levels
        xl=0;
        count=0;
        int pos =1;



        //for (int i = 0; i< column.length; i++) column[i]=null;
        // for (int i=0;i<nodes.length;i++) nodes[i]=null;
        for (int i=0;i<1000;i++) answer[i] = -1;
        //Read the file

        String readLine;

        try {
            BufferedReader br = new BufferedReader(new FileReader(file));

            Item firstIt = new Item();

            firstIt.prev = 0;
            firstIt.next = 0;
            column[0] = firstIt;

            //read the first line of input: items


            readLine = br.readLine();

            String []fst = readLine.split(" ");
            while (fst[0].equals("|")){
                readLine =br.readLine();
                fst=readLine.split(" ");
            }
            //System.out.println("start reading items");
            for (String s : readLine.split(" ")
            ) {
                if (s.equals("|")) {
                   continue;
                }

                // input items
                Item item  = new Item();
                item.name=s;
                item.numNodes=0;
                item.prev =column[0].prev;
                item.next=0;
                column[0].prev=pos;
                column[pos-1].next=pos;
                column[pos]=item;
                //System.out.print(column[pos].name+" ");
                //System.out.println(pos);

                //input first line of Nodes corresponding to items
                Node node = new Node();
                node.up=node.down=node.top =pos;
                nodes[pos]=node;

                pos++;
            }
            // System.out.println("\noptions done");
            // System.out.println(" read remaining nodes");


            //input the remaining items
            int spacerTop =0;
            int spacerUp =pos;

            readLine = br.readLine();
            while (readLine!=null){

                Node spacer = new Node();
                spacer.top =spacerTop;
                spacer.up = pos-spacerUp;
                spacerUp = 0;
                spacerTop--;
                nodes[pos]=spacer;

                for (String s:readLine.split(" ")
                ) {
                    if (s.equals("|")) break;
                    // System.out.print(s+" ");
                    int index = findPosition(s);
                    int indexUp =nodes[index].up;
                    pos++;
                    Node newNode = new Node();
                    newNode.top =index;
                    newNode.down =index;
                    newNode.up = nodes[index].up;
                    nodes[indexUp].down =pos;
                    nodes[index].up = pos;
                    nodes[pos]=newNode;
                    column[index].numNodes++;
                    spacerUp++;

                }
                spacer.down =pos;
                pos++;
                //System.out.println();
                readLine = br.readLine();
            }
            //System.out.println("remaining done");

            Node lastSpacer = new Node();
            lastSpacer.top = spacerTop-1;
            lastSpacer.up = pos-spacerUp;
            lastSpacer.down=-99999;
            nodes[pos] = lastSpacer;
            br.close();


        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
        System.out.println("initialization ... DONE");

        System.out.println(pos+"nodes successfully read");

    }

    public static void algorithmX(){

        initialize();

        boolean isD2 =true;
        boolean isD5 = false;
        boolean isD6 = false;
        boolean isD8 =false;

        while (true){

            if (isD2){
                //System.out.println("D2.....");

                if (column[0].next==0){
                    count++;
                    //System.out.println("all items have been covered, goto D8");
                                        //System.out.println("-----------------------");
                                       // print_answer();
                                       // System.out.println("-----------------------");
                                        isD8 = true;
                                       // break;

                }
                else {
                    it = chose_i();
                    //System.out.println("chose i and cover it "+ it);
                    xl = nodes[it].down;
                    cover(it);
                    isD5 = true;

                }
                isD2 = false;

            }

            if (isD5){
                //System.out.println("D5.......");
               // System.out.println("try xl: "+xl);
                //System.out.println("xl........"+xl+" , "+it+".......");
                if (xl==it){
                    //System.out.println("D7");
                   uncover(it);
                   isD8 = true;
                   isD5 = false;
                }
                else {
                    answer[l]=xl;
                    int p = xl+1;
                    while (p!=xl){
                        int j = nodes[p].top;
                        if (j<=0){
                            p= nodes[p].up;
                        }
                        else{
                            cover(j);
                            p++;
                        }

                    }
                    l++;
                    isD2 = true;
                    isD5 = false;
                    continue;
                }


            }
            if (isD6){
                //System.out.println("D6.....");
                //System.out.println("try again..");
                xl=answer[l];
                int p = xl-1;
                while (p!=xl){
                    int j = nodes[p].top;
                    if (j<=0){
                        p = nodes[p].down;
                    }
                    else {
                        uncover(j);
                        p--;
                    }
                }
                it = nodes[xl].top;
                xl = nodes[xl].down;
                isD5 =true;
                isD6=false;
                continue;

            }
            if (isD8){
                //System.out.println("D8.......");
                if (l==0) {
                   System.out.println("finished with "+count+" solutions");
                    break;

                }
                else {
                    l--;
                    isD6 = true;
                    isD2 = false;


                }

            }

        }


    }
    private static void d_22() {
        boolean isD2 =true;
        while(column[0].next!=0){//
            if (isD2){
                //if (count==10) System.exit(111);
                System.out.println("level ....."+l);
                System.out.println("-------------");
                it = chose_i();
                cover(it);
                xl = nodes[it].down;
                answer[l]=xl;
            }
            //Try xl
            if(xl==it){//Try again
                //Go to D7-D8-D6
                uncover(it);
                l--;
                if (l<0) {
                    System.out.println(count +"answers found");
                    print_answer();break;

                    //System.exit(111);
                }

                //System.out.println("xl before uncovering :"+xl);
                xl=answer[l];
                System.out.println("xl now "+xl);
                //answer[l]=-1;
                int p = xl-1;
                while (p!=xl){
                    if (p==0){
                        System.out.println("you're screwed ,stupid ");
                        return;
                    }
                    // System.out.println("d_6 ..p.."+p);
                    int j=nodes[p].top;
                    if (j<=0) p=nodes[p].down;
                    else {
                        uncover(j);
                        p--;
                    }
                }
                it=nodes[xl].top;
                xl=nodes[xl].down;
                System.out.println(it+":"+xl+"*******************************");
                //d_5();

                isD2=false;
            }
            else {//D5
                System.out.println("entering d5 on level " + l);
                System.out.println("xl is "+xl);
                int p = xl + 1;
                while (p != xl) {
                    int j = nodes[p].top;
                    if (j <= 0) {
                        // System.out.println(p + " was a spacer");
                        p = nodes[p].up;
                    } else {
                        cover(j);
                        p++;
                    }
                }
                answer[l] = xl;
                l++;
                isD2 = true;
                //System.out.println("--------------------");
                //d_2();
            }
            if (column[0].next==0){
                count++;
                //System.out.println(count+"th cover found");

                //print_answer();
                uncover(it);
                l--;
                if (l<0) {
                    System.out.println(count+"answers found");
                    // print_answer();
                    break;
                    //System.exit(404);
                }

                System.out.println("xl before uncovering :"+xl);
                xl=answer[l];
                System.out.println("xl now "+xl);
                //answer[l]=-1;
                int p = xl-1;
                while (p!=xl){
                    if (p==0){
                        System.out.println("you're screwed ,stupid ");
                        return;
                    }
                    System.out.println("d_6 ..p.."+p);
                    int j=nodes[p].top;
                    if (j<=0) p=nodes[p].down;
                    else {
                        uncover(j);
                        p--;
                    }
                }
                it=nodes[xl].top;
                xl=nodes[xl].down;
                // System.out.println(it+":"+xl+"*******************************");
                //d_5();

                isD2=false;


            }



        }
        System.out.println(count+"answers found");
        print_answer();


    }

    private static void print_memory_state() {
        int i = 1;
        while (i <= nodes.length) {
            if (nodes[i] == null) break;
            System.out.println("" + i + " : up: " + nodes[i].up + " down: " + nodes[i].down + " top " + nodes[i].top);
            i++;
        }
    }

    /**
     * print options that contains i
     * @param i the element of the option
     */

    private static void print_option(int i) {

        int p = i+1;
        int q=0;
        while (p!=i){
            if(nodes[p].top<0){
                //System.out.print("("+(-nodes[p].top+1)+")");
                q=p;
                p=nodes[p].up;
                //System.out.println(column[nodes[p].top].name);
                // System.out.print(" |-> ");

                break;
            }


            // System.out.print((nodes[p].top)+" ");
            p++;
        }
        while (p<q){
            System.out.print(column[nodes[p].top].name+" ");
            p++;
        }
        //System.out.println(q+".."+p);

        // System.out.print(nodes[p].top+" . ");

    }

    /*
     * D8: Print final answer; the answer consists of Options
     * that cover the problem
     * */
    static  void print_answer(){
        if (answer[0]==-1) System.out.println("nix gefunden");
        for (int i=0;answer[i]!=-1;i++){
            //System.out.println(answer[i]);
            print_option(answer[i]);
            System.out.println();

        }
    }


    private static void print_items() {
        int i = 0;
        do {
            if (column[i] == null) break;
            System.out.print(" ->  " + column[i].name + " : " + column[i].numNodes);//
            i = column[i].next;
        } while (i != 0);
        System.out.println();
    }


    private static void print_stats() {
        //print_items();
        int i = column[0].next;
        while (i != 0) {
            int j = nodes[i].down;
            System.out.println(i);

            while (j != i) {
                System.out.print(j + " : ");
                j = nodes[j].down;
            }


            System.out.println();
            i = column[i].next;
        }
    }


    private static void cover(int item) {
        //System.out.println( "covering item ..."+item);
        //long startTime = System.nanoTime();
        //System.out.println("start covering item " + item);
        int p = nodes[item].down;
        while (p != item) {
            // System.out.println("my p .."+p);
            hide(p);
            p = nodes[p].down;
        }
        int l, r;
        l = column[item].prev;
        r = column[item].next;
        column[l].next = r;
        column[r].prev = l;
        //long endTime = System.nanoTime();
        //System.out.println("..........................covering time "+(endTime-startTime));
        //System.out.println("cover item " + item + ".....DONE");

        //System.out.println("remaining items after cover: ");
        //print_items();
    }


    private static void uncover(int item) {
        //System.out.println("uncovering item..."+item);
        //long startTime = System.nanoTime();
        if (item == 0) return;
        //System.out.println("start uncovering item " + item);
        int l, r;
        int p = nodes[item].up;
        l = column[item].prev;
        r = column[item].next;
        column[l].next = item;
        column[r].prev = item;
        while (p != item) {
            //System.out.println("unihide --" + p);
            unhide(p);
            p = nodes[p].up;
        }
        long endTime = System.nanoTime();
        //System.out.println(" .......................uncoveing time "+(endTime-startTime));

        //print_one_item(item);
        //System.out.println("uncovering item " + item + "......DONE");
        //System.out.print(" remaining items after uncover:  ");
        //print_items();

    }


    private static void hide(int p) {
        int q = p + 1;
        //System.out.println("my my "+q);
        while (q != p) {
            int x = nodes[q].top;
            int u = nodes[q].up;
            int d = nodes[q].down;
            if (x <= 0) {//q was a spacer
                q = u;
            } else {
                nodes[u].down = d;
                nodes[d].up = u;
                column[x].numNodes--;
                q++;

            }
        }

    }


    private static void unhide(int p) {
        int q = p - 1;
        while (q != p) {
            int x = nodes[q].top;
            int u = nodes[q].up;
            int d = nodes[q].down;
            if (x <= 0) q = d;//q was a spacer
            else {
                nodes[u].down = q;
                nodes[d].up = q;
                column[x].numNodes++;
                q--;

            }

        }

    }


    private static int chose_i() {
        int i = column[0].next;
        int min = Integer.MAX_VALUE;
        int best = Integer.MAX_VALUE;
        do {
            if (column[i] == null) {

                //System.out.println("should never happen" + i);
                break;//-------------------------------------------
            }
            if (min > column[i].numNodes) {
                min = column[i].numNodes;
                best = i;
            }
            //System.out.print(" ->  "+column[i].name+" : "+column[i].numNodes);
            i = column[i].next;
        } while (i != 0);
        //return best;
        //System.out.println(" chosen item on level " + l + " is " + best);
        return best;
    }



    private static void algorithmD() {
        initialize();//D1: Initialization
        print_memory_state();
        System.out.println("------------------");
        d_22();

    }


    public static void main(String[] args) {

        if (0 < args.length) {
            String filename = args[0];
            file = new File(filename);
        } else {
            //Scanner scanner = new Scanner(System.in);
            //System.out.print("Enter a file name: ");
            // System.out.flush();
            //String filename = scanner.nextLine();
            //file=new File("C:\\Users\\epan0\\Desktop\\test.txt");
            file = new File("C:\\Users\\epan0\\OneDrive\\Back Arbeit\\Dancing Links\\dlx1\\octiamond11.txt");
        }
        //algorithmD()
        long startTime = System.currentTimeMillis();
        algorithmX();
        long endTime = System.currentTimeMillis();
        System.out.println("time elapsed: "+(endTime-startTime));
        //print_memory_state();

        // print_answer();
        //d_8();



    }


}