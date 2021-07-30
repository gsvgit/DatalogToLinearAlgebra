// Implemented as described by Ashu M. G. Solo in "Multidimensional Matrix Mathematics: Multidimensional Matrix Equality, Addition, Subtraction, and Multiplication, Part 2 of 6"
// http://www.iaeng.org/publication/WCE2010/WCE2010_pp1829-1833.pdf
//
// I suppose that all dimensions have equal size. In Datalog, we have a finite number of atoms and this number specifies the size of matrices.
let multiply (m1:'t[,,], m1_d1, m1_d2) (m2:'t[,,], m2_d1, m2_d2) =
    let n = m1.GetLength 0
    let res = Array3D.zeroCreate n n n
    let n = n - 1
    for i in 0..n do
        for j in 0..n do
            for k in 0..n do
                for l in 0..n do
                    let m1_actual = if m1_d2 = 2 then m1.[i,l,k] else m1.[i,j,l]
                    let m2_actual = if m2_d1 = 1 then m2.[l,j,k] else m2.[i,l,k]
                    res.[i,j,k] <- res.[i,j,k] + m1_actual * m2_actual

    res



let print (m:'t[,,]) =
    let n = m.GetLength 0 - 1
    for i in 0..n do
        for j in 0..n do
            for k in 0..n do
                printfn "(%i,%i,%i) = %A" i j k m.[i,j,k]

//
// An example 1: VPT(v,t) :- U(r,v,t),R(r)
//
// Set up R.
// 1-st dimension is for 'r'
// 2-nd dimension is for 'q' --- magic dimension to unify dimensions of different matrices
// 3-rd dimension is for 'v' --- also quiet magic but it becomes obvious if analyze result structure.
let R =
    let a = Array3D.zeroCreate 4 4 4
    a.[1,0,0] <- 1
    a.[1,0,1] <- 1 // because 'r' and 'v' is independent
    a.[1,0,2] <- 1 // because 'r' and 'v' is independent
    a.[1,0,3] <- 1 // because 'r' and 'v' is independent
    a

// Set up U. Actually, transposed.
// 1-st dimension is for 't'
// 2-nd dimension is for 'r'
// 3-rd dimension is for 'v'
let U =
    let a = Array3D.zeroCreate 4 4 4
    a.[0,1,2] <- 1
    a.[1,1,3] <- 1
    a.[1,3,2] <- 1
    a

//
// An example 2: S(x,y) :- A(x,y), B(y,x)
// To express this example we should use 3d matrices instead of 2d.
//
// Set up A. It should be expanded to 3d by introducing 'z'.
let A =
    let a = Array3D.zeroCreate 4 4 4
    a.[0,1,0] <- 1
    a.[0,1,1] <- 1
    a.[0,1,2] <- 1
    a.[0,1,3] <- 1

    a.[2,0,0] <- 1
    a.[2,0,1] <- 1
    a.[2,0,2] <- 1
    a.[2,0,3] <- 1

    a

// Set up B. It should be expanded to 3d by duplicating 'y'.
let B =
    let a = Array3D.zeroCreate 4 4 4
    a.[1,1,2] <- 1

    a.[1,1,3] <- 1

    a.[3,3,0] <- 1

    a

let runExample name f =
    printfn ""
    printfn "=========================="
    printfn "%s" name
    printfn "=========================="
    let res = f()
    print res

[<EntryPoint>]
let main argv =

    // Evaluate example 1
    // VPT is a multiplication of U and R alongside 1-st and 2-nd dimensions
    runExample "Example 1" (fun _ -> multiply (U,1,2) (R,1,2))

    // Evaluate example 2
    // Actually, S is a projection of a multiplication of A and B alongside 1-st and 2-nd dimensions, but we omit projection to clarify evaluation result.
    runExample "Example 2" (fun _ -> multiply (A,1,2) (B,1,2))

    0 // return an integer exit code
