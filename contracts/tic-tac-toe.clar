;; to have atrack of the game
(define-data-var latest-game-id uint u0)

(define-map games uint {
    player-one: principal,
    player-two: (optional principal),
    is-player-one-turn: bool,
    bet-amount: uint,
    board: (list 9 uint),
    winner: (optional principal),
})

(define-private (validate-move (board (list 9 uint)) (move-index uint) (move uint))
    (let
        (
            (index-in-range (and (>= move-index u0) (< move-index u9)))

            (x-or-o (or (is-eq move u1) (is-eq move u2)))

            (empty-spot (is-eq (unwrap! (element-at? board move-index) false) u0))
        )

        (and (is-eq index-in-range true) (is-eq x-or-o true) empty-spot)
    )
)

(define-constant THIS_CONTRACT (as-contract tx-sender)) 
(define-constant ERR_MIN_BET_AMOUNT u100) 
(define-constant ERR_INVALID_MOVE u101) 
(define-constant ERR_GAME_NOT_FOUND u102) 
(define-constant ERR_GAME_CANNOT_BE_JOINED u103) 
(define-constant ERR_NOT_YOUR_TURN u104) 

;; Creading a new game:
(define-public (create-game (bet-amount uint) (move-index uint) (move uint))
    (let (
        
        (game-id (var-get latest-game-id)) ;; 0
        
        (starting-board (list u0 u0 u0 u0 u0 u0 u0 u0 u0))
        
        (game-board (unwrap! (replace-at? starting-board move-index move) (err ERR_INVALID_MOVE)))
        
        (game-data {
            player-one: contract-caller,
            player-two: none,
            is-player-one-turn: false,
            bet-amount: bet-amount,
            board: game-board,
            winner: none
        })
    )

    (asserts! (> bet-amount u0) (err ERR_MIN_BET_AMOUNT)) 
    
    (asserts! (is-eq move u1) (err ERR_INVALID_MOVE))
    
    (asserts! (validate-move starting-board move-index move) (err ERR_INVALID_MOVE))

   
    (try! (stx-transfer? bet-amount contract-caller THIS_CONTRACT)) 
    
    (map-set games game-id game-data)
    
    (var-set latest-game-id (+ game-id u1))

    
    (print { action: "create-game", data: game-data})
    
    (ok game-id)
))


;; function to joing a game:
(define-public (join-game (game-id uint) (move-index uint) (move uint))
    (let (
        
        (original-game-data (unwrap! (map-get? games game-id) (err ERR_GAME_NOT_FOUND)))
        
        (original-board (get board original-game-data))

        
        (game-board (unwrap! (replace-at? original-board move-index move) (err ERR_INVALID_MOVE)))
        
        (game-data (merge original-game-data {
            board: game-board,
            player-two: (some contract-caller),
            is-player-one-turn: true
        }))
    )

    
    (asserts! (is-none (get player-two original-game-data)) (err ERR_GAME_CANNOT_BE_JOINED)) 
    
    (asserts! (is-eq move u2) (err ERR_INVALID_MOVE))
    
    (asserts! (validate-move original-board move-index move) (err ERR_INVALID_MOVE))

    
    (try! (stx-transfer? (get bet-amount original-game-data) contract-caller THIS_CONTRACT))
    
    (map-set games game-id game-data)

 
    (print { action: "join-game", data: game-data})
    
    (ok game-id)
))


(define-private (is-line (board (list 9 uint)) (a uint) (b uint) (c uint)) 
    (let (
        
        (a-val (unwrap! (element-at? board a) false))
        
        (b-val (unwrap! (element-at? board b) false))
        
        (c-val (unwrap! (element-at? board c) false))
    )

    (and (is-eq a-val b-val) (is-eq a-val c-val) (not (is-eq a-val u0)))
))


(define-private (has-won (board (list 9 uint))) 
    (or 
        (is-line board u0 u1 u2) 
        (is-line board u3 u4 u5) 
        (is-line board u6 u7 u8) 
        (is-line board u0 u3 u6) 
        (is-line board u1 u4 u7) 
        (is-line board u2 u5 u8) 
        (is-line board u0 u4 u8) 
        (is-line board u2 u4 u6) 
    )
)

(define-public (play (game-id uint) (move-index uint) (move uint))
    (let (
        
        (original-game-data (unwrap! (map-get? games game-id) (err ERR_GAME_NOT_FOUND)))
        
        (original-board (get board original-game-data))

   
        (is-player-one-turn (get is-player-one-turn original-game-data))
        
        (player-turn (if is-player-one-turn (get player-one original-game-data) (unwrap! (get player-two original-game-data) (err ERR_GAME_NOT_FOUND))))
        
        (expected-move (if is-player-one-turn u1 u2))

        
        (game-board (unwrap! (replace-at? original-board move-index move ) (err ERR_INVALID_MOVE)))
        
        (is-now-winner (has-won game-board))
        

        (game-data (merge original-game-data {
            board: game-board,
            is-player-one-turn: (not is-player-one-turn), 
            winner: (if is-now-winner (some player-turn) none)
        }))
    )

    
    (asserts! (is-eq player-turn contract-caller) (err ERR_NOT_YOUR_TURN))
    
    (asserts! (is-eq move expected-move) (err ERR_INVALID_MOVE))
  
    (asserts! (validate-move original-board move-index move) (err ERR_INVALID_MOVE))

    
    (if is-now-winner (try! (as-contract (stx-transfer? (* u2 (get bet-amount game-data)) tx-sender player-turn))) false)

    
    (map-set games game-id game-data)

    
    (print {action: "play", data: game-data})

    (ok game-id)
))