open Core

open Basic
open Stats

exception N_too_small

(*
  Computes the Jarque-Bera statistic for the given dataset.

  Note: for n >= 2,000 the distribution of the JB statistic
  approximates a X^2 (Chi-squared) distribution with 2 degrees of
  freedom.

  Warning: this function will throw the N_too_small exception if the
  dataset is smaller than 2,000.

  References:
  1. "The Shapiro-Wilk And Related Tests for Normality",
     [https://math.mit.edu/~rmd/465/shapiro.pdf]
*)
let jarque_bera ~xs =
  let n =
    let n = Array.length xs in
    if n < 2000 then raise N_too_small;
    float n
  in
  let u = mean xs in
  (* compute the biased sample deviation *)
  let s = sqrt (sum ~f:(fun x -> Float.square (x -. u)) xs /. n) in
  let skew = sum ~f:(fun x -> pow_int ((x -. u) /. s) 3) xs /. n in
  let kurt = (sum ~f:(fun x -> pow_int ((x -. u) /. s) 4) xs /. n) -. 3.0 in
  n /. 6.0 *. (Float.square skew +. (Float.square kurt /. 4.0))

(**
  Computes an approximation of the r-th largest value in a random
  sample of n values drawn from a Gaussian distribution having
  the given mean and standard deviation sample.

  Note: this function uses an approximation proposed by Blom (1958).

  Warning: for small n, this approximation is rather rough.

  References:
  1. [Reference Table](https://projecteuclid.org/journalArticle/Download?urlId=10.1214%2Faoms%2F1177728266)
  2. [Algorithm AS 177: Expected Normal Order Statistics (Exact and Approximate)](https://www.jstor.org/stable/2347982 )
*)
let order_stat_approx ~r ~n ~mean ~std : float =
  let alpha = 0.375 in
  mean
  +. (cdf_gaussian_pinv ~x:((float r -. alpha) /. (float n -. (2.0 *. alpha) +. 1.0)) ~sigma:1.0 *. std)

let%expect_test "order_stat_1" =
  printf "%.1f" @@ order_stat_approx ~r:20 ~n:20 ~mean:0.0 ~std:1.0;
  [%expect {|1.9|}]

(**
  Computes an approximation of mean value of the r-th largest
  values in a random sample of n values drawn from a Gaussian
  distribution with mean 0 and standard deviation 1.

  References:
  1. [Algorithm AS 177: Expected Normal Order Statistics (Exact and Approximate)](https://www.jstor.org/stable/2347982 )
  2. D. Teichroew, et. al. "Tables of Expected Values of Order
     Statistics and Products of Order Statistics for Samples of
     Size Twenty and Less from the Normal Distribution".
     Numerical Analysis Research, University of California,
     Los Angeles. 
     https://projecteuclid.org/journalArticle/Download?urlId=10.1214%2Faoms%2F1177728266
  3. [Exptected Values of Normal Order Statistics](http://faculty.washington.edu/heagerty/Books/Biostatistics/TABLES/NormalOrder.pdf)
*)
let order_stat ~r ~n : float =
  let open Float in
  float r * binomial ~n r * (Integrate.qagi () ~f:(fun x ->
    x * pow_int (1.0 - Erf.erf_q x) Int.(r - 1) * pow_int (Erf.erf_q x) Int.(n - r) * pdf_normal ~mean:0.0 ~std:1.0 x)
  ).out

let%expect_test "order_stat_1" =
  printf "%.4f" (order_stat ~r:8 ~n:10);
  [%expect {|0.6561|}]

let%expect_test "order_stat_2" =
  printf "%.4f" (order_stat ~r:2 ~n:4);
  [%expect {|-0.2970|}]

(*
  Computes the Shapiro Francia statistic which parameterizes
  the likelihood that the given sample is drawn from a Gaussian
  distribution with mean 0 and standard deviation 1.
*)
let shapiro_francia_stat (xs : float array) : float =
  let n = Array.length xs in
  let m = mean xs in
  Array.sort ~len:n xs ~compare:[%compare: float];
  let os = Array.init n ~f:(fun r -> order_stat ~r ~n) in
  sumi xs ~f:(fun r y -> (y -. m) *. os.(r))
  /. Float.sqrt
       (sumi xs ~f:(fun _ y -> Float.square (y -. m)) *. sumi xs ~f:(fun r _ -> Float.square os.(r)))

(*
  let%expect_test "shapiro_francia_stat_1" =
    printf "%.4f"
      (shapiro_francia_stat
         [|
           0.664220;
           -0.631950;
           -0.448290;
           0.184850;
           -1.40500;
           0.896160;
           -0.598050;
           -0.425810;
           0.504560;
           0.732380;
           1.91910;
           -0.0628270;
           0.451500;
           -0.581380;
           -1.07650;
           -0.245060;
           0.204370;
           -0.646910;
           -0.007770;
           -1.47800;
           -0.573960;
           0.448420;
           -1.25420;
           0.220640;
           -1.18590;
           -1.14360;
           -0.890480;
           -0.90406;
           1.24900;
           -0.875340;
           -0.0253890;
           -0.0810450;
           1.87970;
           0.930510;
           0.865260;
           -0.618640;
           0.110770;
           -2.00530;
           0.328060;
           0.593620;
         |]);
    [%expect {||}]
*)

(*
  Returns the normalized Shapiro Francia Statistics.

  Note: According to P. Royston "A Toolkit for Testing
  for Non-Normality in Complete and Censored Samples", this
  value is normally distributed with mean:

    mean: ln(ln(n)) - ln(n)
    std: ln(ln(n)) + 2/ln(n)

  where n denotes the sample size.

  This transformation was originally reported by Roysten (1992a)
  for n values from 5 to 5000.

  Warning: this transformation is only valid for n values ranging
  from 5 to 5000.

  References:
  1. ["A Toolkit for Testing for Non-Normality in Complete and Censored Samples"]( https://www.jstor.org/stable/2348109)
*)
let shapiro_francia_test (xs : float array) : float =
  let n = float (Array.length xs) in
  let mean = Float.log (Float.log n) -. Float.log n in
  let std = Float.log (Float.log n) +. (2.0 /. Float.log n) in
  let w = Float.log (1.0 -. shapiro_francia_stat xs) in
  let p = cdf_gaussian_p ~x:(w -. mean) ~std in
  let q = 1.0 -. p in
  if Float.(p >= 0.5) then p -. q else q -. p

(*
  let%expect_test "shapiro_francia_test" =
    printf "%.4f"
      (shapiro_francia_test
         [|
           0.664220;
           -0.631950;
           -0.448290;
           0.184850;
           -1.40500;
           0.896160;
           -0.598050;
           -0.425810;
           0.504560;
           0.732380;
           1.91910;
           -0.0628270;
           0.451500;
           -0.581380;
           -1.07650;
           -0.245060;
           0.204370;
           -0.646910;
           -0.007770;
           -1.47800;
           -0.573960;
           0.448420;
           -1.25420;
           0.220640;
           -1.18590;
           -1.14360;
           -0.890480;
           -0.90406;
           1.24900;
           -0.875340;
           -0.0253890;
           -0.0810450;
           1.87970;
           0.930510;
           0.865260;
           -0.618640;
           0.110770;
           -2.00530;
           0.328060;
           0.593620;
         |]);
    [%expect {||}]
*)
(*
  let%expect_test "shapiro_francia_test_2" =
    let () = Random.init 5 in
    let xs = Array.init 30 ~f:(fun _ -> Random.float 3.0) in
    printf "%.4f" (shapiro_francia_test xs);
    [%expect {||}]
*)
