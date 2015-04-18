module TreeTests
open TreeWorkflow
open NUnit.Framework
//I'll be testing 'big' functions (filter, map, calc), 
//'cause all of the functions from TreeWorkflow (Bind, Combine and etc) have been used in them
[<Test>]
let ``filter test 1`` () =
  let a = Node (4, Nil, Node (3, Nil, Nil))
  let res = (filterTree a 
                        (fun x -> (x % 2) = Node (1, Nil, Nil)))
  Assert.AreEqual (res, Node (3, Nil, Nil))
[<Test>]
let ``filter test 2`` () =
  let a = Node (-3, Nil, Node (1, Node (-2, Nil, Nil), Node (2, Nil, Nil)))
  let res = (filterTree a (fun x -> x < 0))
  Assert.AreEqual (res, Node (-2, Node (-3, Nil, Nil), Nil))

[<Test>]
let ``map test 1: many Nils become one`` () =
  let a = Node (10, Node (1, Nil, Nil), Node (1000, Node (100, Nil, Nil), Node (10000, Nil, Nil)))
  let res = (mapTree a (fun x -> x / 0))
  printfn "%A" res
  Assert.AreEqual (res, (Nil : tree<int>))

[<Test>]
let ``map test 2: return value should be BST`` () =
  let a = Node (7, Node (1, Nil, Nil), Node (8, Node (10, Nil, Nil), Nil))
  let res = (mapTree a (fun x -> x % 4))
  printfn "%A" res
  Assert.AreEqual (res, Node (0, Nil, Node (2, Node (1, Nil, Nil), Node (3, Nil, Nil))))

[<Test>]
let ``calc test 1`` () =
  let a = Node (10, Nil, Node (11, Nil, Nil))
  let b = Node (1000, Node (0, Nil, Nil), Nil)
  let res = calcTree a b (+)
  Assert.AreEqual(res, Node (10,Nil,Node (11,Nil,Node (1011,Node (1010,Nil,Nil),Nil))))

[<Test>]
let ``calc test 2`` () =
  let a = Node (1000, Nil, Node (11, Nil, Nil))
  let b = Node (10, Node (0, Nil, Nil), Nil)
  let res = calcTree a b (/)
  Assert.AreEqual(res, Node (1,Nil,Node (100,Nil,Nil)))