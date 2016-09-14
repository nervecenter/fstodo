type TaskStatus =
    | Done
    | Working
    | Todo

type Task = {
    Num     : int;
    Status  : TaskStatus;
    Text    : string;
}

type QueryStatus =
    | Done
    | Working
    | Todo
    | All

(*
 *  COMMANDS AND THEIR PARSING
 *)

type Command =
    | Add of string
    | Delete of int
    | Query of QueryStatus
    | Reset
    | Change of int * TaskStatus
    | Error of string

let usageString = "\
    To use fstodo, utilize the following syntax rules:\n\
    \tAdd a task:                   fstask add \"Task to be added\"\n\
    \tDelete a task:                fstask del [#]\n\
    \tQuery tasks:                  fstask [todo/working/done/all]\n\
    \tDelete tasks, start fresh:    fstask reset\n\
    \tChange a task's status:       fstask [#] [todo/working/done]\n\
    "

let parseAdd rest =
    match rest with
    | taskString :: _ -> Add taskString
    | _               -> Error usageString

let parseDelete rest =
    match rest with
    | num :: _ -> try Delete Int32.Parse(num) 
                  with _ -> Error usageString
    | _ -> Error usageString

let parseChange num rest =
    match rest with
    | "todo"    :: _ -> Change (num, Todo)
    | "working" :: _ -> Change (num, Working)
    | "done"    :: _ -> Change (num, Done)
    | _              -> Error usageString

let parseCommand argv : Command =
    match argv with
    | "add"     :: rest -> parseAdd rest
    | "del"     :: rest -> parseDelete rest
    | "todo"    :: _    -> Query Todo
    | "working" :: _    -> Query Working
    | "done"    :: _    -> Query Done
    | "all"     :: _    -> Query All
    | "reset"   :: _    -> Reset
    | num       :: rest -> try parseChange Int32.Parse(num) rest
                           with _ -> Error usageString
    | _                 -> Error usageString

(*
 *  READING TASKS FROM FILE
 *)

type TaskFileRead =
    | Success of Task list
    | Failure

type TaskParse =
    | Success of Task
    | Failure

let parseTask str =
    match str.Split [|' '|] with
    | num :: status :: rest -> let text = String.concat " " rest
                               match status with
                               | "d" ->
                               | "w" ->
                               | "t" ->
    | 


let readTaskListFromFile loc =
    match IO.Exists(loc) with
    | true -> try
                  seq {
                      use sr = new StreamReader (loc)
                      while not sr.EndOfStream do
                          yield sr.ReadLine ()
                  }
                  |> Seq.toList
                  |> List.map parseTask
              with
                  _ -> Failure
    | false -> Success []

(*
 * TODO LIST FUNCTIONALITY
 *)

type TodoActionResult =
    | Success
    | Failure

let executeCommand command taskList =
    match command with
    | Add str
    | Delete num
    | Query status
    | Reset
    | Change num status
    | Error str

(*
 *  MAIN ENTRYPOINT
 *)

[<EntryPoint>]
let main argv =
    let commandResult  = parseCommand argv
    match commandResult with
    | Error str -> printfn "%S" str
                   0
    | command -> match taskListResult with
                 | Success li -> match executeCommand command li with
                                 | Success -> 0
                                 | Failure -> printfn ""
                 | Failure -> printfn "Error reading tasks file."
                              0
    let taskListResult = readTaskListFromFile "~/.tasks"
