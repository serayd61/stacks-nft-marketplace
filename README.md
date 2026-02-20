# Stacks Smart Contract Platform

A comprehensive collection of **25 production-ready smart contracts** for the Stacks blockchain, built with Clarity 3.0. Includes a modern Next.js frontend for browsing and deploying contracts.

![Stacks Contract Deployer](https://img.shields.io/badge/Contracts-25-purple)
![Clarity](https://img.shields.io/badge/Clarity-3.0-blue)
![License](https://img.shields.io/badge/License-MIT-green)

## 🚀 Live Demo

Deploy the frontend to Vercel and start exploring contracts.

## 📦 Contract Categories

### 🖼️ NFT Contracts (5)
| Contract | Description |
|----------|-------------|
| `nft-marketplace` | Full marketplace with fixed-price and auction support |
| `nft-collection` | SIP-009 compliant NFT with minting and royalties |
| `nft-staking` | Stake NFTs to earn rewards |
| `nft-rental` | Rent NFTs with collateral protection |
| `nft-fractional` | Fractionalize NFTs into fungible tokens |

### 🪙 Token Contracts (5)
| Contract | Description |
|----------|-------------|
| `fungible-token` | SIP-010 compliant token with mint/burn |
| `token-vesting` | Linear and cliff vesting schedules |
| `token-airdrop` | Batch token distribution with whitelist |
| `token-swap` | Atomic swaps between tokens |
| `token-bridge` | Cross-chain bridge with oracle verification |

### 💰 DeFi Contracts (5)
| Contract | Description |
|----------|-------------|
| `lending-protocol` | Collateralized lending with liquidation |
| `liquidity-pool` | AMM with constant product formula |
| `yield-farming` | Stake LP tokens to earn rewards |
| `staking-pool` | Flexible/locked staking with tiered APY |
| `flash-loan` | Uncollateralized same-tx loans |

### 🏛️ DAO Contracts (5)
| Contract | Description |
|----------|-------------|
| `dao-governance` | Proposal creation and voting |
| `dao-treasury` | Multi-sig treasury management |
| `dao-voting` | Multiple voting mechanisms |
| `dao-membership` | Tiered membership system |
| `dao-proposals` | Advanced proposal templates |

### 🔧 Utility Contracts (5)
| Contract | Description |
|----------|-------------|
| `escrow-service` | P2P escrow with dispute resolution |
| `subscription-service` | Recurring payments management |
| `crowdfunding` | Campaign creation with reward tiers |
| `lottery` | Provably fair lottery system |
| `multisig-wallet` | Multi-signature wallet |

## 🛠️ Installation

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed
- [Node.js](https://nodejs.org/) 18+ for frontend
- [Git](https://git-scm.com/)

### Clone Repository
```bash
git clone https://github.com/serayd61/stacks-nft-marketplace.git
cd stacks-nft-marketplace
```

### Run Contract Tests
```bash
clarinet test
```

### Check Contracts
```bash
clarinet check
```

## 🌐 Frontend Setup

```bash
cd frontend
npm install
npm run dev
```

Open [http://localhost:3000](http://localhost:3000) to view the contract browser.

## 🚀 Deploy to Vercel

### Option 1: Vercel CLI
```bash
cd frontend
npm i -g vercel
vercel
```

### Option 2: GitHub Integration
1. Push to GitHub
2. Import project in [Vercel Dashboard](https://vercel.com/new)
3. Set root directory to `frontend`
4. Deploy

### Environment Variables
```
NEXT_PUBLIC_NETWORK=testnet  # or mainnet
```

## 📝 Contract Deployment

### Deploy Single Contract
```bash
clarinet contract deploy nft-marketplace
```

### Deploy to Testnet
```bash
clarinet deployments generate --testnet
clarinet deployments apply -p deployments/default.testnet-plan.yaml
```

### Deploy to Mainnet
```bash
clarinet deployments generate --mainnet
clarinet deployments apply -p deployments/default.mainnet-plan.yaml
```

## 🔧 Customization

Before deploying, customize these parameters in contracts:

### Common Parameters
- `contract-owner` - Admin address
- `platform-fee` - Fee percentage (basis points)
- `treasury` - Fee recipient address

### NFT Marketplace
```clarity
(define-constant platform-fee u250) ;; 2.5%
(define-constant treasury 'SP...) ;; Your address
```

### Token Settings
```clarity
(define-constant max-supply u1000000000000000) ;; 1B tokens
(define-constant token-decimals u6)
```

## 📚 Documentation

- [Clarity Language Reference](https://docs.stacks.co/clarity/language-reference)
- [Stacks Documentation](https://docs.stacks.co)
- [Clarinet Documentation](https://docs.hiro.so/clarinet)

## 🧪 Testing

Each contract includes comprehensive tests:

```bash
# Run all tests
clarinet test

# Run specific test file
clarinet test tests/marketplace_test.ts

# Run with coverage
clarinet test --coverage
```

## 📁 Project Structure

```
stacks-nft-marketplace/
├── contracts/           # 25 Clarity smart contracts
│   ├── nft-*.clar      # NFT contracts
│   ├── token-*.clar    # Token contracts
│   ├── lending-*.clar  # DeFi contracts
│   ├── dao-*.clar      # DAO contracts
│   └── *.clar          # Utility contracts
├── tests/              # Contract tests
├── frontend/           # Next.js frontend
│   ├── app/           # App router pages
│   ├── components/    # React components
│   └── lib/           # Utilities
├── Clarinet.toml      # Clarinet configuration
└── README.md
```

## 🤝 Contributing

Contributions are welcome! Please read [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

1. Fork the repository
2. Create feature branch (`git checkout -b feature/amazing-feature`)
3. Commit changes (`git commit -m 'Add amazing feature'`)
4. Push to branch (`git push origin feature/amazing-feature`)
5. Open Pull Request

## 🔒 Security

- All contracts should be audited before mainnet deployment
- Review [SECURITY.md](SECURITY.md) for security considerations
- Report vulnerabilities via GitHub Security Advisories

## 📄 License

MIT License - see [LICENSE](LICENSE) for details.

## 🙏 Acknowledgments

- [Stacks Foundation](https://stacks.org)
- [Hiro Systems](https://hiro.so)
- Stacks Developer Community

---

**Built with ❤️ for the Stacks ecosystem**
