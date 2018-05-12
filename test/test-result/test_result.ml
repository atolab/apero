open Apero

module TestA = struct
  module Error = struct
    type e = AOne | ATwo
  end
end

module TestB = struct
  module Error = struct
    type e = BOne | BTwo
  end
end


module ResultA = Result.Make(TestA.Error)
module ResultB = Result.Make(TestB.Error)
module ABConv = Result.Converter(ResultA)(ResultB)

let result_a () = ResultA.ok 10

let result_b () = ResultB.ok 20

let error_a () = ResultA.fail TestA.Error.AOne

let error_b () = ResultB.fail TestB.Error.BOne

let result_a2b e = ABConv.lift_e e (fun _ -> TestB.Error.BOne)

let test_result () = let _ = result_a2b (error_a ()) in ()

let test_battery = [
  "Result" , `Quick, test_result;
]

(* Run it *)
let () =
  Alcotest.run "Apero Result Test" [
    "test_result", test_battery;
  ]
