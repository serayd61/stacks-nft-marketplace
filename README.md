# Stacks NFT Marketplace

A decentralized NFT marketplace on Stacks blockchain supporting fixed-price listings and English auctions.

## Features

- **Fixed Price Listings**: List NFTs at a set price
- **English Auctions**: Timed auctions with minimum bid increments
- **Reserve Prices**: Set minimum acceptable price for auctions
- **Platform Fees**: 2.5% fee on successful sales
- **Seller/Buyer Stats**: Track trading history
- **Price Updates**: Modify listing prices
- **Expiring Listings**: Auto-expire after set duration

## Smart Contract Functions

### Fixed Price Sales
```clarity
(list-nft (nft-contract) (token-id) (price) (duration))  ;; Create listing
(buy-nft (listing-id))                                    ;; Purchase NFT
(cancel-listing (listing-id))                             ;; Cancel listing
(update-price (listing-id) (new-price))                   ;; Update price
```

### Auctions
```clarity
(create-auction (nft-contract) (token-id) (start-price) (reserve-price) (duration))
(place-bid (auction-id) (bid-amount))
(end-auction (auction-id))
```

### Read-Only
```clarity
(get-listing (listing-id))
(get-auction (auction-id))
(get-marketplace-stats)
(get-seller-stats (seller))
(get-buyer-stats (buyer))
(calculate-fee (price))
(is-listing-active (listing-id))
(is-auction-active (auction-id))
```

## Fee Structure

| Type | Fee |
|------|-----|
| Platform Fee | 2.5% |
| Listing Fee | Free |
| Auction Fee | Free |

## Usage Examples

### List an NFT
```clarity
;; List NFT for 100 STX, expires in ~7 days (10080 blocks)
(contract-call? .nft-marketplace list-nft 
  'SP123...nft-contract 
  u1 
  u100000000 
  u10080)
```

### Buy an NFT
```clarity
;; Buy listing #5
(contract-call? .nft-marketplace buy-nft u5)
```

### Create an Auction
```clarity
;; Auction starting at 10 STX, reserve 50 STX, 3 days duration
(contract-call? .nft-marketplace create-auction 
  'SP123...nft-contract 
  u1 
  u10000000 
  u50000000 
  u4320)
```

### Place a Bid
```clarity
;; Bid 25 STX on auction #3
(contract-call? .nft-marketplace place-bid u3 u25000000)
```

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                  NFT Marketplace Contract                │
├─────────────────────────────────────────────────────────┤
│  Fixed Listings                                          │
│  ├── listing-id -> seller, price, expiry                 │
│  └── active status                                       │
├─────────────────────────────────────────────────────────┤
│  Auctions                                                │
│  ├── auction-id -> seller, bids, timing                  │
│  ├── current-bid, highest-bidder                         │
│  └── reserve-price                                       │
├─────────────────────────────────────────────────────────┤
│  Stats                                                   │
│  ├── seller-stats                                        │
│  ├── buyer-stats                                         │
│  └── marketplace totals                                  │
└─────────────────────────────────────────────────────────┘
```

## Security

- Only listing owner can cancel or update
- Auction bids require minimum increment (1 STX)
- Reserve price protection for sellers
- Automatic expiration for stale listings
- Platform fees sent to transparent treasury

## Installation

```bash
git clone https://github.com/serayd61/stacks-nft-marketplace.git
cd stacks-nft-marketplace
clarinet test
```

## License

MIT License

## Contributing

Contributions welcome!



---
## Status (December 2024)
- ✅ NFT listing and buying on mainnet
- ✅ Royalty support
- ✅ STX payments
