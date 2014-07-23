import Data.List

main = do
  let ans = [x*y*z | m <- [1..1000], n <- [1..999], m < n,
              let x = n^2 - m^2,
              let y = 2 * n * m,
              let z = n^2 + m^2,
              x + y + z == 1000]
  putStrLn (show ans)
