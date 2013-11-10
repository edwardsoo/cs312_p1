-- Name: Edward Soo, Annie Li
-- Student Number: 71680094, 

-- Returns the best next move
-- board: current state of the game, represented by an array of rows
-- me: 'w' or 'b'
-- moves: the number of moves to search ahead
oska_b0e7 board me moves = min_max_root_b0e7 board me moves

min_max_root_b0e7 state me h =
  snd (tuple_max_b0e7 (zip (map (min_max_b0e7 me (h-1) False) states) states) (-100 * length state, []))
  where states = gen_state_b0e7 me state

min_max_b0e7::Char -> Int -> Bool -> [String] -> Int
min_max_b0e7 me h myTurn state
  | h == 0 = evaluate_state_b0e7 me state
  | myTurn = maximum (map (min_max_b0e7 me (h-1) False) (gen_state_b0e7 me state))
  | otherwise = minimum (map (min_max_b0e7 me (h-1) True) (gen_state_b0e7 (opponent_b0e7 me) state))

tuple_max_b0e7 [] lead = lead
tuple_max_b0e7 (x:xs) lead
  | (fst x) > (fst lead) = tuple_max_b0e7 xs x
  | otherwise = tuple_max_b0e7 xs lead

-- Print all neighbor states and thie eval values
print_moves_evals_b0e7:: [String] -> Char-> IO()
print_moves_evals_b0e7 state me =
  print_states_evals_b0e7 (gen_state_b0e7 me state) me  

-- Prints a list of states and their eval values
print_states_evals_b0e7:: [[String]] -> Char -> IO()
print_states_evals_b0e7 [] me = putStrLn ""
print_states_evals_b0e7 (s:ss) me = do
  print_state_eval s me
  putStrLn ""
  print_states_evals_b0e7 ss me 

-- Prints a state and its eval value
print_state_eval state me = do
  print_b_b0e7 state
  putStr "eval: "
  putStrLn (show (evaluate_state_b0e7 me state))

-- Prints a state
print_b_b0e7 state = disp_b_b0e7 state (length state)
disp_b_b0e7 [] n = putStr ""
disp_b_b0e7 (x:xs) n = do
  putStr [' ' | i <- [1..n - (length x)]]
  putStrLn (concat [c:' ':[] | c <- x])
  disp_b_b0e7 xs n 

-- Evaluates a state and returns an integer
evaluate_state_b0e7 'w' state = evaluate_state_b0e7' state 'w'
evaluate_state_b0e7 'b' state = evaluate_state_b0e7' (reverse state) 'b'

-- Helper function, assumes my side starts from the top
evaluate_state_b0e7' state me
  | won_b0e7 state me = (length state) * 2
  | won_b0e7 reversed (opponent_b0e7 me) = (length state) * -2
  | otherwise = (sum_rows_eval_b0e7 [] (head state) (tail state) me (length state)) -
    (sum_rows_eval_b0e7 [] (head reversed) (tail reversed) (opponent_b0e7 me) (length reversed))
  where reversed = reverse state

-- Determine if I have won in a state
won_b0e7 state me =
  no_piece_b0e7 state (opponent_b0e7 me) || 
  all_remaining_done_b0e7 state me

-- Returns true if all my remaining pieces are on the opponent's start row
all_remaining_done_b0e7 state me =
  no_piece_b0e7 (tail reversed) me && count_b0e7 (head reversed) me > 0
  where reversed = reverse state

-- Returns true if board no longer has pieces of side
no_piece_b0e7::[String] -> Char -> Bool
no_piece_b0e7 state side = foldl (count_piece_in_row_b0e7 side) 0 state == 0
count_piece_in_row_b0e7::Char -> Int -> String -> Int
count_piece_in_row_b0e7 side acc row = acc + count_b0e7 row side

-- Evaluates the state row by row
sum_rows_eval_b0e7:: [String] -> String -> [String] -> Char -> Int -> Int
sum_rows_eval_b0e7 pre this [] me h = eval_row_b0e7 pre this [] me h
sum_rows_eval_b0e7 pre this post me h =
  (eval_row_b0e7 pre this post me h) +
  (sum_rows_eval_b0e7 (this:pre) (head post) (tail post) me h)

-- Evaluates one row with regards to what is on its next and prev row
eval_row_b0e7::[String] -> String -> [String] -> Char -> Int -> Int
eval_row_b0e7 [] this post me h =
  (count_b0e7 this me) + (h * (count_block_move_b0e7 (head post) this (opponent_b0e7 me)))
eval_row_b0e7 pre this [] me h =
  (count_b0e7 this me)*h
eval_row_b0e7 pre this post me h
  | length pre == length post = (eval_row_function_b0e7 (head pre) this (head post) me h weight)
  | length pre > length post = (eval_row_function_b0e7 (pad_b0e7 (head pre)) this (head post) me h weight)
  | otherwise = (eval_row_function_b0e7 (head pre) this (pad_b0e7 (head post)) me h weight)
  where weight = (length pre) + 1

-- Evaluate by summing the weighted count of 
-- 1. my pieces on this row
-- 2. opponent's piece on the next row that cannot attack
-- 3. my pieces on this row that is under attack
eval_row_function_b0e7 front this back me h weight =
  (count_blocked_opp_b0e7 front this back False me) * (h - weight) -
  (count_being_atk_b0e7 front this back me) * (h - weight + 2) +
  (count_b0e7 this me) * weight

-- Returns the number of opponents on the next row that cannot attack this row
count_blocked_opp_b0e7 _ [] _ prevJmp me = 0
count_blocked_opp_b0e7 (bl:br:bs) (r:rs) (fl:fr:fs) prevJmp me
  | prevJmp && (blocked_b0e7 fl r br me) = 1 +
    (count_blocked_opp_b0e7 (br:bs) rs (fr:fs) ((blocked_b0e7 fr r bl me)) me)
  | otherwise = (count_blocked_opp_b0e7 (br:bs) rs (fr:fs) ((blocked_b0e7 fr r bl me)) me)

-- Returns the number of pieces on this row that are under attack
count_being_atk_b0e7 _ [] _ me = 0
count_being_atk_b0e7 (bl:br:bs) (r:rs) (fl:fr:fs) me
  | (being_atk fl r br me) || (being_atk fr r bl me) = 1 + 
    (count_being_atk_b0e7 (br:bs) rs (fr:fs) me)
  | otherwise = (count_being_atk_b0e7 (br:bs) rs (fr:fs) me)
  
-- returns True if front cannot jump to back and capture this
blocked_b0e7 front this back me 
  | front == (opponent_b0e7 me) && this == me && back /= '-' = True
  | otherwise = False

-- returns True if front can jump to back and capture this
being_atk front this back me
  | front == (opponent_b0e7 me) && this == me && back == '-' = True
  | otherwise = False

append_b0e7 [] e = [e]
append_b0e7 (x:xs) e = x:append_b0e7 xs e

pad_b0e7 list = '!':(append_b0e7 list '!')

-- returns the number of an element in a list
count_b0e7 [] e = 0
count_b0e7 (x:xs) e
  | x == e = 1 + count_b0e7 xs e
  | otherwise = count_b0e7 xs e

-- count the number of my pieces that are blocked by opponents
count_block_move_b0e7 this next me =
  count_surrounded_b0e7 [] (head interleaved) (tail interleaved) me (opponent_b0e7 me)
  where interleaved = interleave_b0e7 this next

-- count the number of x that are surrounded by y or end of lst
count_surrounded_b0e7 [] this right x y
  | head right == y && this == x = 1 + count_surrounded_b0e7 (this:[]) (head right) (tail right) x y
  | otherwise = count_surrounded_b0e7 (this:[]) (head right) (tail right) x y
count_surrounded_b0e7 left this [] x y
  | head left == y && this == x = 1
  | otherwise = 0
count_surrounded_b0e7 (l:ls) this (r:rs) x y
  | l == y && this == x && r == y = 1 + count_surrounded_b0e7 (this:l:ls) r rs x y
  | otherwise = count_surrounded_b0e7 (this:l:ls) r rs x y

interleave_b0e7 [] next = next
interleave_b0e7 this [] = this
interleave_b0e7 this next
  | length this > length next = head this:head next :interleave_b0e7 (tail this) (tail next)
  | otherwise = head next : head this : interleave_b0e7 (tail this) (tail next)

-- returns a list of tuple {eval, state}, 
-- where eval is the output of the static board eval function and 
-- state is a neighbor state of the current state of the board
gen_eval_state_tuple_b0e7 me board = 
  zip (map (evaluate_state_b0e7 me) states) states 
  where states = gen_state_b0e7 me board

-- Use this function to generate all possible moves of current state
-- result does not include the current state; can be empty
gen_state_b0e7 me board
  | me == 'w' = gen_state_all_row_b0e7 [] board 'w'
  | otherwise = map reverse (gen_state_all_row_b0e7 [] (reverse board) 'b')

-- returns all possible states generated by making all legal moves of the board
gen_state_all_row_b0e7 pre [] me = []
gen_state_all_row_b0e7 pre post me =
  gen_state_row_b0e7 pre row (tail post) me ++
  gen_state_all_row_b0e7 (reverse(row:reverse pre)) (tail post) me
  where row = head post

-- returns all possible states generated by making all legal moves of a row
gen_state_row_b0e7 pre row post me
  -- last row
  | null post  = []
  -- second last row
  | length post == 1 = 
    gen_state_row_move_b0e7 pre row (head post) (tail post) me 0
  -- has more at least 2 rows below
  | otherwise = 
    gen_state_row_move_b0e7_jump_b0e7 pre row (head post) (head (tail post)) (tail (tail post)) me 0

-- returns all possible states generated by making all legal moves of a row when the row only has one other row below it
gen_state_row_move_b0e7 pre row nextRow post me j
  | j >= length row = []
  | length row > length nextRow =
    gen_state_upper_piece_move_left_b0e7 j pre row nextRow post me++
    gen_state_upper_piece_move_right_b0e7 j pre row nextRow post me++
    gen_state_row_move_b0e7 pre row nextRow post me (j+1)
  | otherwise =
    gen_state_lower_piece_move_left_b0e7 j pre row nextRow post me++
    gen_state_lower_piece_move_right_b0e7 j pre row nextRow post me++
    gen_state_row_move_b0e7 pre row nextRow post me (j+1)
  
-- returns all possible states generated by making all legal moves of a row when the row has at least two rows below it
gen_state_row_move_b0e7_jump_b0e7 pre row nextRow nextNextRow post me j
  | j >= length row = []
  | length row == length nextNextRow =
    gen_state_upper_piece_move_left_b0e7 j pre row nextRow (nextNextRow:post) me ++
    gen_state_upper_piece_move_right_b0e7 j pre row nextRow (nextNextRow:post) me ++
    gen_state_mid_piece_jump_left_b0e7 j pre row nextRow nextNextRow post me ++
    gen_state_mid_piece_jump_right_b0e7 j pre row nextRow nextNextRow post me ++
    gen_state_row_move_b0e7_jump_b0e7 pre row nextRow nextNextRow post me (j+1)
  | length row > length nextNextRow = 
    gen_state_upper_piece_move_left_b0e7 j pre row nextRow (nextNextRow:post) me ++
    gen_state_upper_piece_move_right_b0e7 j pre row nextRow (nextNextRow:post) me ++
    gen_state_upper_piece_jump_left_b0e7 j pre row nextRow nextNextRow post me ++
    gen_state_upper_piece_jump_right_b0e7 j pre row nextRow nextNextRow post me ++
    gen_state_row_move_b0e7_jump_b0e7 pre row nextRow nextNextRow post me (j+1)
  | otherwise =
    gen_state_lower_piece_move_left_b0e7 j pre row nextRow (nextNextRow:post) me ++
    gen_state_lower_piece_move_right_b0e7 j pre row nextRow (nextNextRow:post) me ++
    gen_state_lower_piece_jump_left_b0e7 j pre row nextRow nextNextRow post me ++
    gen_state_lower_piece_jump_right_b0e7 j pre row nextRow nextNextRow post me ++
    gen_state_row_move_b0e7_jump_b0e7 pre row nextRow nextNextRow post me (j+1)
  
-- For the side that starts on top
-- If (i < n-2), a chess piece on [i][j] can move to [i+1][j] or [i+1][j-1]
gen_state_upper_piece_move_left_b0e7 j pre row nextRow post me 
  | (j > 0) && (row !! j == me) && (nextRow !! (j-1) == '-') = 
    [pre++[(replace_b0e7 row j '-'), (replace_b0e7 nextRow (j-1) me)]++post]
  | otherwise = []
gen_state_upper_piece_move_right_b0e7 j pre row nextRow post me 
  | (j < length nextRow) && (row !! j == me) && (nextRow !! j == '-') = 
    [pre++[(replace_b0e7 row j '-'), (replace_b0e7 nextRow j me)]++post]
  | otherwise = []

-- If (i >= n-2), a chess piece on [i][j] can move to [i+1][j] or [i+1][j+1]
gen_state_lower_piece_move_left_b0e7 j pre row nextRow post me 
  | (row !! j == me) && (nextRow !! j == '-') = 
    [pre++[(replace_b0e7 row j '-'), (replace_b0e7 nextRow j me)]++post]
  | otherwise = []
gen_state_lower_piece_move_right_b0e7 j pre row nextRow post me 
  | (row !! j == me) && (nextRow !! (j+1) == '-') = 
    [pre++[(replace_b0e7 row j '-'), (replace_b0e7 nextRow (j+1) me)]++post]
  | otherwise = []

-- If (i < n-3), a chess piece on [i][j] can jump to [i+2][j] and capture an opponent_b0e7 piece on [i+1][j], or jump to [i+2][j-2] and capture a piece on [i+1][j-1]
gen_state_upper_piece_jump_left_b0e7 j pre row nextRow nextNextRow post me
  | (j > 1) && (row !! j == me) && (nextRow !! (j-1) == (opponent_b0e7 me)) && 
      (nextNextRow !! (j-2) == '-')=
    [pre++[replace_b0e7 row j '-', replace_b0e7 nextRow (j-1) '-', replace_b0e7 nextNextRow (j-2) me]++post]
  | otherwise = []
gen_state_upper_piece_jump_right_b0e7 j pre row nextRow nextNextRow post me
  | (j < length(nextNextRow)) && (row !! j == me) && 
      (nextRow !! j == (opponent_b0e7 me)) && (nextNextRow !! j  == '-') =
    [pre++[replace_b0e7 row j '-', replace_b0e7 nextRow j '-', replace_b0e7 nextNextRow j me]++post]
  | otherwise = []

-- If (i == n-3), a chess piece on [i][j] can jump to [i+2][j+1] and capture a piece on [i+1][j], or jump to [i+2][j-1] and capture [i+2][j-1]
gen_state_mid_piece_jump_left_b0e7 j pre row nextRow nextNextRow post me
  | (j > 0) && (row !! j == me) && (nextRow !! (j-1) == (opponent_b0e7 me)) && 
      (nextNextRow !! (j-1) == '-') =
    [pre++[replace_b0e7 row j '-', replace_b0e7 nextRow (j-1) '-', replace_b0e7 nextNextRow (j-1) me]++post]
  | otherwise = []
gen_state_mid_piece_jump_right_b0e7 j pre row nextRow nextNextRow post me
  | (j < length(nextRow)) && (row !! j == me) && 
      (nextRow !! j == (opponent_b0e7 me)) && (nextNextRow !! (j+1)  == '-') =
    [pre++[replace_b0e7 row j '-', replace_b0e7 nextRow j '-', replace_b0e7 nextNextRow (j+1) me]++post]
  | otherwise = []

-- If (i > n-3), a piece on [i][j] can jump to [i+2][j] and capture [i+1][j], or jump to [i+2][j+2] and capture [i+1][j+1]
gen_state_lower_piece_jump_left_b0e7 j pre row nextRow nextNextRow post me
  | (row !! j == me) && (nextRow !! j == (opponent_b0e7 me)) && 
      (nextNextRow !! j == '-')=
    [pre++[replace_b0e7 row j '-', replace_b0e7 nextRow j '-', replace_b0e7 nextNextRow j me]++post]
  | otherwise = []
gen_state_lower_piece_jump_right_b0e7 j pre row nextRow nextNextRow post me
  | (row !! j == me) && (nextRow !! (j+1) == (opponent_b0e7 me)) && 
      (nextNextRow !! (j+2)  == '-') =
    [pre++[replace_b0e7 row j '-', replace_b0e7 nextRow (j+1) '-', replace_b0e7 nextNextRow (j+2) me]++post]
  | otherwise = []

-- Replace the the i-th element of list with new
replace_b0e7 list i new
  | null list = []
  | i == 0 = new:tail list
  | otherwise = head list:replace_b0e7 (tail list) (i-1) new

opponent_b0e7 me 
  | me == 'w' = 'b'
  | otherwise = 'w'
