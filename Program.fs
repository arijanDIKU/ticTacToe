open System open System.IO


module Board = 
   type Cell = Free | X | O
   type Board = Cell list 
   type Index = int*int

   let initializeBoard () : Board =
      let init = [for i in 1..9 do Free] in init:Board

   let updateBoard (board:Board) ((c,r):Index) (newCell:Cell) : Board = 
      let indexIntoList = (r-1)+(c/6)
      board |> List.mapi (fun i x -> if i=indexIntoList then newCell else x)       

   let isFull (board:Board) : bool = 
      board |> List.exists (fun c -> c=Free) |> not    

   let getCell (board:Board) (cell:Index): Cell =
      let c,r=cell 
      board.[(r-1)+(c/6)] 

   let writeBoard (board:Board) : unit = 
      let format = function | Free -> "     " | X -> "  X  " | O -> "  O  "
      let bottom = let b = "_____|" in b+b+b 
      let empty = let e = "     |" in e+e+e
      Console.SetCursorPosition(0,0)
      board |> List.splitInto 3 |> List.iter (fun row -> match row with 
                                                         | [] -> ()
                                                         | [x;y;z] -> printfn "%s\n%s|%s|%s|\n%s" empty (format x) (format y) (format z) bottom
                                                         | _ -> () ) 
      
module Game = 
   open Board 

   type Player = PlayerX | PlayerO 
   type State = Winner | Ongoing | Unresolved   
    


   let (|InBound|_|) (cki:ConsoleKeyInfo) (curPos:Index) = 
      let key = cki.Key 
      if key = ConsoleKey.DownArrow    && snd curPos=7  then None
      elif key = ConsoleKey.UpArrow    && snd curPos=1  then None   
      elif key = ConsoleKey.LeftArrow  && fst curPos=2  then None   
      elif key = ConsoleKey.RightArrow && fst curPos=14 then None   
      else Some InBound 

   let threeInARow board : bool*Cell = 
      let greatestCell,count = board |> List.countBy id |> List.maxBy snd 
      if greatestCell=Free || count<3 then (false, Free)  
      else let threes = let rows = [for t in 0..2 do board.[t*3..t*3+2]] in 
                        let cols = [ board.[0]::(board.[3]::(board.[6]::([]))) ] @ [ board.[2]::(board.[5]::(board.[8]::([]))) ]
                                    @ [ board.[1]::(board.[4]::(board.[7]::([]))) ] in
                                   [ [board.[0];board.[4];board.[8]] ] @ [ [board.[2];board.[4];board.[6]] ] @ rows @ cols          //all possible winning scenarios
           match greatestCell with 
           | X -> if threes |> List.contains [X;X;X] then (true, X) else (false, X)
           | O -> if threes |> List.contains [O;O;O] then (true, O) else (false, O)
           | _ -> (false,Free) 
         


   let rec readInput (curPos:Index) (board:Board) (player:Player) : Board*Player =
      let cki = Console.ReadKey(true) //true = don't print to console
      match cki.Key with 
      | ConsoleKey.X -> let cell = getCell board curPos in if cell=Free && player=PlayerX then updateBoard board curPos X, PlayerO
                                                           else Console.SetCursorPosition(0,10); printf "Slot is occupied, look for another slot!"; 
                                                                Console.SetCursorPosition(curPos); readInput curPos board player 
      | ConsoleKey.O -> let cell = getCell board curPos in if cell=Free && player=PlayerO then updateBoard board curPos O, PlayerX
                                                           else Console.SetCursorPosition(0,10); printf "Slot is occupied, look for another slot!"; 
                                                                Console.SetCursorPosition(curPos); readInput curPos board player 
  
      |_ -> match curPos with
            | InBound cki -> match cki.Key with                                                                            //setcursorpos uses row,col format  
                             | ConsoleKey.RightArrow -> let newPos = fst curPos+6, snd curPos in Console.SetCursorPosition(newPos)
                                                        readInput newPos board player
                             | ConsoleKey.LeftArrow ->  let newPos = fst curPos-6, snd curPos in Console.SetCursorPosition(newPos)
                                                        readInput newPos board player
                             | ConsoleKey.UpArrow ->    let newPos = fst curPos, snd curPos-3 in Console.SetCursorPosition(newPos); 
                                                        readInput newPos board player
                             | ConsoleKey.DownArrow ->  let newPos = fst curPos, snd curPos+3 in Console.SetCursorPosition(newPos); 
                                                        readInput newPos board player
                             | _ ->  Console.SetCursorPosition(0,11); printf "!"
                                     Console.SetCursorPosition(curPos); readInput curPos board player 


            | _ -> Console.SetCursorPosition(0,10); printf "Only arrow keys, X or O is valid input"
                   Console.SetCursorPosition(curPos); readInput curPos board player
  
   let eval (board:Board) : State = 
      match threeInARow board with 
      | true, X -> Winner 
      | true, O -> Winner
      | false, _ -> if isFull board then Unresolved else Ongoing 

   let rec play (c:int) ((board,player):Board*Player) : unit =
      writeBoard board 
      Console.SetCursorPosition(0,12); printfn "%A: %i: %A" player c board
      
      match eval board with
      | Winner -> writeBoard board; printfn "%A won!: %i" player c
      | Unresolved -> writeBoard board; printfn "No winner!" 
      | Ongoing -> readInput (8,4) board player |> play (c+1)
      
 
   let beginGame () : unit =
      Console.Clear()
      let board = initializeBoard ()
      play 0 (board,PlayerO) 
               
open Game

[<EntryPoint>]
let main argv =
    beginGame ()
    0 // return an integer exit code

