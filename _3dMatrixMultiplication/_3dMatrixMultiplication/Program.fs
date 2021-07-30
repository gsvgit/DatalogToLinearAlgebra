// Implemented as described by Ashu M. G. Solo in "Multidimensional Matrix Mathematics: Multidimensional Matrix Equality, Addition, Subtraction, and Multiplication, Part 2 of 6"
// http://www.iaeng.org/publication/WCE2010/WCE2010_pp1829-1833.pdf
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

// An example: VPT(v,t) :- U(r,v,t),R(r)

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

let print (m:'t[,,]) =
    let n = m.GetLength 0 - 1
    for i in 0..n do
        for j in 0..n do
            for k in 0..n do
                printfn "(%i,%i,%i) = %A" i j k m.[i,j,k]

[<EntryPoint>]
let main argv =
    //VPT is a multiplication of U and R alongside 1-st and 2-nd dimensions
    let res = multiply (U,1,2) (R,1,2)
    print res
    0 // return an integer exit code
