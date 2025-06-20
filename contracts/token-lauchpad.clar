;; =============================================================================
;; COMMUNITY TOKEN LAUNCHPAD
;; =============================================================================
;; A comprehensive platform for launching community tokens with fair distribution,
;; vesting schedules, governance setup, and liquidity provision tools.

;; =============================================================================
;; CONSTANTS & ERROR CODES
;; =============================================================================

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_ALREADY_EXISTS (err u101))
(define-constant ERR_NOT_FOUND (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_SALE_NOT_ACTIVE (err u105))
(define-constant ERR_SALE_ENDED (err u106))
(define-constant ERR_VESTING_NOT_STARTED (err u107))
(define-constant ERR_NOTHING_TO_CLAIM (err u108))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u109))
(define-constant ERR_VOTING_ENDED (err u110))
(define-constant ERR_ALREADY_VOTED (err u111))
(define-constant ERR_INSUFFICIENT_TOKENS (err u112))

;; =============================================================================
;; DATA STRUCTURES
;; =============================================================================

;; Token launch configuration
(define-map token-launches
  { token-id: uint }
  {
    creator: principal,
    token-name: (string-ascii 32),
    token-symbol: (string-ascii 10),
    total-supply: uint,
    price-per-token: uint,
    sale-start: uint,
    sale-end: uint,
    vesting-duration: uint,
    vesting-cliff: uint,
    min-purchase: uint,
    max-purchase: uint,
    tokens-sold: uint,
    funds-raised: uint,
    is-active: bool,
    governance-enabled: bool
  }
)

;; User purchases and vesting
(define-map user-purchases
  { token-id: uint, user: principal }
  {
    total-purchased: uint,
    total-claimed: uint,
    last-claim-block: uint,
    purchase-block: uint
  }
)

;; Governance proposals
(define-map proposals
  { token-id: uint, proposal-id: uint }
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    voting-start: uint,
    voting-end: uint,
    yes-votes: uint,
    no-votes: uint,
    executed: bool,
    proposal-type: (string-ascii 20)
  }
)

;; User votes on proposals
(define-map user-votes
  { token-id: uint, proposal-id: uint, user: principal }
  { vote: bool, voting-power: uint }
)

;; Liquidity pool reserves
(define-map liquidity-pools
  { token-id: uint }
  {
    stx-reserve: uint,
    token-reserve: uint,
    total-liquidity: uint,
    creator-fee: uint
  }
)

;; User liquidity positions
(define-map liquidity-positions
  { token-id: uint, user: principal }
  { liquidity-tokens: uint, last-reward-block: uint }
)

;; =============================================================================
;; DATA VARIABLES
;; =============================================================================

(define-data-var next-token-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var platform-fee-rate uint u250) ;; 2.5% in basis points
(define-data-var min-governance-threshold uint u1000000) ;; 1M tokens minimum for governance

;; =============================================================================
;; TOKEN LAUNCH FUNCTIONS
;; =============================================================================

;; Launch a new community token
(define-public (launch-token
  (token-name (string-ascii 32))
  (token-symbol (string-ascii 10))
  (total-supply uint)
  (price-per-token uint)
  (sale-duration uint)
  (vesting-duration uint)
  (vesting-cliff uint)
  (min-purchase uint)
  (max-purchase uint)
  (enable-governance bool))
  (let
    (
      (token-id (var-get next-token-id))
      (sale-start (+ stacks-block-height u1))
      (sale-end (+ stacks-block-height sale-duration))
    )
    (asserts! (> total-supply u0) ERR_INVALID_AMOUNT)
    (asserts! (> price-per-token u0) ERR_INVALID_AMOUNT)
    (asserts! (> sale-duration u0) ERR_INVALID_AMOUNT)
    (asserts! (<= min-purchase max-purchase) ERR_INVALID_AMOUNT)

    (map-set token-launches
      { token-id: token-id }
      {
        creator: tx-sender,
        token-name: token-name,
        token-symbol: token-symbol,
        total-supply: total-supply,
        price-per-token: price-per-token,
        sale-start: sale-start,
        sale-end: sale-end,
        vesting-duration: vesting-duration,
        vesting-cliff: vesting-cliff,
        min-purchase: min-purchase,
        max-purchase: max-purchase,
        tokens-sold: u0,
        funds-raised: u0,
        is-active: true,
        governance-enabled: enable-governance
      }
    )

    (var-set next-token-id (+ token-id u1))
    (print { event: "token-launched", token-id: token-id, creator: tx-sender })
    (ok token-id)
  )
)

;; Purchase tokens during sale period
(define-public (purchase-tokens (token-id uint) (token-amount uint))
  (let
    (
      (launch-data (unwrap! (map-get? token-launches { token-id: token-id }) ERR_NOT_FOUND))
      (current-block stacks-block-height)
      (cost (* token-amount (get price-per-token launch-data)))
      (existing-purchase (default-to
        { total-purchased: u0, total-claimed: u0, last-claim-block: u0, purchase-block: u0 }
        (map-get? user-purchases { token-id: token-id, user: tx-sender })))
      (total-purchase-amount (+ (get total-purchased existing-purchase) token-amount))
    )

    ;; Validation checks
    (asserts! (get is-active launch-data) ERR_SALE_NOT_ACTIVE)
    (asserts! (>= current-block (get sale-start launch-data)) ERR_SALE_NOT_ACTIVE)
    (asserts! (< current-block (get sale-end launch-data)) ERR_SALE_ENDED)
    (asserts! (>= token-amount (get min-purchase launch-data)) ERR_INVALID_AMOUNT)
    (asserts! (<= total-purchase-amount (get max-purchase launch-data)) ERR_INVALID_AMOUNT)
    (asserts! (<= (+ (get tokens-sold launch-data) token-amount) (get total-supply launch-data)) ERR_INSUFFICIENT_BALANCE)

    ;; Transfer payment
    (try! (stx-transfer? cost tx-sender (as-contract tx-sender)))

    ;; Update purchase record
    (map-set user-purchases
      { token-id: token-id, user: tx-sender }
      {
        total-purchased: total-purchase-amount,
        total-claimed: (get total-claimed existing-purchase),
        last-claim-block: (get last-claim-block existing-purchase),
        purchase-block: (if (is-eq (get total-purchased existing-purchase) u0) current-block (get purchase-block existing-purchase))
      }
    )

    ;; Update launch data
    (map-set token-launches
      { token-id: token-id }
      (merge launch-data
        {
          tokens-sold: (+ (get tokens-sold launch-data) token-amount),
          funds-raised: (+ (get funds-raised launch-data) cost)
        }
      )
    )

    (print { event: "tokens-purchased", token-id: token-id, buyer: tx-sender, amount: token-amount })
    (ok true)
  )
)

;; =============================================================================
;; VESTING & CLAIMING FUNCTIONS
;; =============================================================================

;; Calculate claimable tokens based on vesting schedule
(define-read-only (get-claimable-tokens (token-id uint) (user principal))
  (let
    (
      (launch-data (unwrap! (map-get? token-launches { token-id: token-id }) ERR_NOT_FOUND))
      (purchase-data (unwrap! (map-get? user-purchases { token-id: token-id, user: user }) ERR_NOT_FOUND))
      (current-block stacks-block-height)
      (vesting-start (+ (get purchase-block purchase-data) (get vesting-cliff launch-data)))
      (vesting-end (+ vesting-start (get vesting-duration launch-data)))
    )
    (if (< current-block vesting-start)
      (ok u0)
      (if (>= current-block vesting-end)
        (ok (- (get total-purchased purchase-data) (get total-claimed purchase-data)))
        (let
          (
            (vesting-progress (- current-block vesting-start))
            (total-vesting-blocks (get vesting-duration launch-data))
            (vested-amount (/ (* (get total-purchased purchase-data) vesting-progress) total-vesting-blocks))
          )
          (ok (- vested-amount (get total-claimed purchase-data)))
        )
      )
    )
  )
)

;; Claim vested tokens
(define-public (claim-tokens (token-id uint))
  (let
    (
      (claimable (try! (get-claimable-tokens token-id tx-sender)))
      (purchase-data (unwrap! (map-get? user-purchases { token-id: token-id, user: tx-sender }) ERR_NOT_FOUND))
    )
    (asserts! (> claimable u0) ERR_NOTHING_TO_CLAIM)

    ;; Update claim record
    (map-set user-purchases
      { token-id: token-id, user: tx-sender }
      (merge purchase-data
        {
          total-claimed: (+ (get total-claimed purchase-data) claimable),
          last-claim-block: stacks-block-height
        }
      )
    )

    (print { event: "tokens-claimed", token-id: token-id, user: tx-sender, amount: claimable })
    (ok claimable)
  )
)

;; =============================================================================
;; GOVERNANCE FUNCTIONS
;; =============================================================================

;; Create a governance proposal
(define-public (create-proposal
  (token-id uint)
  (title (string-ascii 100))
  (description (string-ascii 500))
  (voting-duration uint)
  (proposal-type (string-ascii 20)))
  (let
    (
      (launch-data (unwrap! (map-get? token-launches { token-id: token-id }) ERR_NOT_FOUND))
      (proposal-id (var-get next-proposal-id))
      (user-tokens (get-user-token-balance token-id tx-sender))
    )
    (asserts! (get governance-enabled launch-data) ERR_UNAUTHORIZED)
    (asserts! (>= user-tokens (var-get min-governance-threshold)) ERR_INSUFFICIENT_TOKENS)

    (map-set proposals
      { token-id: token-id, proposal-id: proposal-id }
      {
        proposer: tx-sender,
        title: title,
        description: description,
        voting-start: stacks-block-height,
        voting-end: (+ stacks-block-height voting-duration),
        yes-votes: u0,
        no-votes: u0,
        executed: false,
        proposal-type: proposal-type
      }
    )

    (var-set next-proposal-id (+ proposal-id u1))
    (print { event: "proposal-created", token-id: token-id, proposal-id: proposal-id })
    (ok proposal-id)
  )
)

;; Vote on a proposal
(define-public (vote-on-proposal (token-id uint) (proposal-id uint) (vote bool))
  (let
    (
      (proposal-data (unwrap! (map-get? proposals { token-id: token-id, proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
      (user-tokens (get-user-token-balance token-id tx-sender))
      (current-block stacks-block-height)
    )
    (asserts! (>= current-block (get voting-start proposal-data)) ERR_UNAUTHORIZED)
    (asserts! (< current-block (get voting-end proposal-data)) ERR_VOTING_ENDED)
    (asserts! (> user-tokens u0) ERR_INSUFFICIENT_TOKENS)
    (asserts! (is-none (map-get? user-votes { token-id: token-id, proposal-id: proposal-id, user: tx-sender })) ERR_ALREADY_VOTED)

    ;; Record vote
    (map-set user-votes
      { token-id: token-id, proposal-id: proposal-id, user: tx-sender }
      { vote: vote, voting-power: user-tokens }
    )

    ;; Update proposal vote counts
    (map-set proposals
      { token-id: token-id, proposal-id: proposal-id }
      (if vote
        (merge proposal-data { yes-votes: (+ (get yes-votes proposal-data) user-tokens) })
        (merge proposal-data { no-votes: (+ (get no-votes proposal-data) user-tokens) })
      )
    )

    (print { event: "vote-cast", token-id: token-id, proposal-id: proposal-id, voter: tx-sender, vote: vote })
    (ok true)
  )
)

;; =============================================================================
;; LIQUIDITY PROVISION FUNCTIONS
;; =============================================================================

;; Add liquidity to token pool
(define-public (add-liquidity (token-id uint) (stx-amount uint) (max-token-amount uint))
  (let
    (
      (launch-data (unwrap! (map-get? token-launches { token-id: token-id }) ERR_NOT_FOUND))
      (pool-data (default-to
        { stx-reserve: u0, token-reserve: u0, total-liquidity: u0, creator-fee: u0 }
        (map-get? liquidity-pools { token-id: token-id })))
      (user-position (default-to
        { liquidity-tokens: u0, last-reward-block: stacks-block-height }
        (map-get? liquidity-positions { token-id: token-id, user: tx-sender })))
    )

    (asserts! (> stx-amount u0) ERR_INVALID_AMOUNT)

    (let
      (
        (token-amount (if (is-eq (get stx-reserve pool-data) u0)
          max-token-amount
          (/ (* stx-amount (get token-reserve pool-data)) (get stx-reserve pool-data))))
        (liquidity-minted (if (is-eq (get total-liquidity pool-data) u0)
          stx-amount
          (/ (* stx-amount (get total-liquidity pool-data)) (get stx-reserve pool-data))))
      )

      (asserts! (<= token-amount max-token-amount) ERR_INVALID_AMOUNT)
      (asserts! (>= (get-user-token-balance token-id tx-sender) token-amount) ERR_INSUFFICIENT_TOKENS)

      ;; Transfer assets
      (try! (stx-transfer? stx-amount tx-sender (as-contract tx-sender)))

      ;; Update pool reserves
      (map-set liquidity-pools
        { token-id: token-id }
        {
          stx-reserve: (+ (get stx-reserve pool-data) stx-amount),
          token-reserve: (+ (get token-reserve pool-data) token-amount),
          total-liquidity: (+ (get total-liquidity pool-data) liquidity-minted),
          creator-fee: (get creator-fee pool-data)
        }
      )

      ;; Update user position
      (map-set liquidity-positions
        { token-id: token-id, user: tx-sender }
        {
          liquidity-tokens: (+ (get liquidity-tokens user-position) liquidity-minted),
          last-reward-block: stacks-block-height
        }
      )

      (print { event: "liquidity-added", token-id: token-id, user: tx-sender, stx-amount: stx-amount, token-amount: token-amount })
      (ok { liquidity-minted: liquidity-minted, token-amount: token-amount })
    )
  )
)

;; =============================================================================
;; READ-ONLY FUNCTIONS
;; =============================================================================

;; Get token launch details
(define-read-only (get-token-launch (token-id uint))
  (map-get? token-launches { token-id: token-id })
)

;; Get user purchase details
(define-read-only (get-user-purchase (token-id uint) (user principal))
  (map-get? user-purchases { token-id: token-id, user: user })
)

;; Get user's effective token balance (claimed + claimable)
(define-read-only (get-user-token-balance (token-id uint) (user principal))
  (match (map-get? user-purchases { token-id: token-id, user: user })
    purchase-data (+ (get total-claimed purchase-data)
                    (unwrap-panic (get-claimable-tokens token-id user)))
    u0
  )
)

;; Get proposal details
(define-read-only (get-proposal (token-id uint) (proposal-id uint))
  (map-get? proposals { token-id: token-id, proposal-id: proposal-id })
)

;; Get liquidity pool info
(define-read-only (get-liquidity-pool (token-id uint))
  (map-get? liquidity-pools { token-id: token-id })
)

;; Get user liquidity position
(define-read-only (get-user-liquidity (token-id uint) (user principal))
  (map-get? liquidity-positions { token-id: token-id, user: user })
)

;; =============================================================================
;; ADMIN FUNCTIONS
;; =============================================================================

;; Emergency pause token sale
(define-public (pause-token-sale (token-id uint))
  (let ((launch-data (unwrap! (map-get? token-launches { token-id: token-id }) ERR_NOT_FOUND)))
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get creator launch-data))) ERR_UNAUTHORIZED)

    (map-set token-launches
      { token-id: token-id }
      (merge launch-data { is-active: false })
    )
    (ok true)
  )
)

;; Update platform fee
(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set platform-fee-rate new-fee)
    (ok true)
  )
)

;; Withdraw platform fees
(define-public (withdraw-fees (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
    (ok true)
  )
)
