open System


module Board = 
   type Cell = Free | X | O
   type Board = Cell list 
   type Index = int*int

   let initializeBoard () : Board =
      let init = [for i in 1..9 do Free] in init:Board

   let updateBoard (board:Board) ((c,r):Index) (newCell:Cell) : Board = 
      let indexIntoList = (r/3)+(c/6)
      board |> List.mapi (fun i x -> if i=indexIntoList then newCell else x)       

   let isFull (board:Board) : bool = 
      board |> List.exists (fun c -> c=Free) |> not    

   let getCell (board:Board) (cell:Index): Cell =
      let c,r=cell 
      board.[(r/3)+(c/6)] 

   let writeBoard (board:Board) : unit = 
      let format = function | Free -> "     " | X -> "  X  " | O -> "  O  "
      let bottom = let b = "_____|" in b+b+b 
      let empty = let e = "     |" in e+e+e
      Console.SetCursorPosition(0,0)
      board |> List.splitInto 3 |> List.iter (fun row -> match row with 
                                                         | [] -> ()
                                                         | [x;y;z] -> printfn "%s\n%s|%s|%s|\n%s" empty (format x) (format y) (format z) bottom
                                                         | _ -> () ) 
      



let updateBoard ((c,r):int*int) : int = 
   (r/3)+(c/6)

while true do
   let input = Console.ReadLine()
   let x = int(input.[0])
   let y = int(input.[2])
   printfn "%A" (updateBoard (x,y))
    


