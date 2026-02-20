export interface Contract {
  id: string;
  name: string;
  description: string;
  category: 'nft' | 'token' | 'defi' | 'dao' | 'utility';
  features: string[];
  fileName: string;
}

export const contracts: Contract[] = [
  // NFT Contracts
  {
    id: 'nft-marketplace',
    name: 'NFT Marketplace',
    description: 'Full-featured marketplace with fixed-price listings and English auctions. Includes platform fees, seller/buyer stats, and expiring listings.',
    category: 'nft',
    features: ['Fixed Price Sales', 'English Auctions', 'Reserve Prices', '2.5% Platform Fee', 'Seller/Buyer Stats'],
    fileName: 'nft-marketplace.clar'
  },
  {
    id: 'nft-collection',
    name: 'NFT Collection',
    description: 'SIP-009 compliant NFT collection with minting, burning, royalties, and metadata support. Perfect for launching your own NFT project.',
    category: 'nft',
    features: ['SIP-009 Compliant', 'Batch Minting', 'Royalty Support', 'Metadata URIs', 'Approval System'],
    fileName: 'nft-collection.clar'
  },
  {
    id: 'nft-staking',
    name: 'NFT Staking',
    description: 'Stake NFTs to earn rewards over time. Features tiered bonuses for longer lock periods and whitelisted NFT collections.',
    category: 'nft',
    features: ['Stake to Earn', 'Lock Period Bonuses', 'Whitelist System', 'Reward Pool', 'Auto-compound'],
    fileName: 'nft-staking.clar'
  },
  {
    id: 'nft-rental',
    name: 'NFT Rental',
    description: 'Rent NFTs for a specified duration with collateral protection. Great for gaming NFTs and utility tokens.',
    category: 'nft',
    features: ['Time-based Rental', 'Collateral System', 'Auto-expiry', 'Rental Stats', 'Flexible Pricing'],
    fileName: 'nft-rental.clar'
  },
  {
    id: 'nft-fractional',
    name: 'NFT Fractional',
    description: 'Fractionalize expensive NFTs into fungible tokens. Enables shared ownership and buyout mechanisms.',
    category: 'nft',
    features: ['Fractionalization', 'Buyout Mechanism', 'Share Trading', 'Ownership %', 'Redemption'],
    fileName: 'nft-fractional.clar'
  },

  // Token Contracts
  {
    id: 'fungible-token',
    name: 'Fungible Token',
    description: 'SIP-010 compliant fungible token with minting, burning, and transfer controls. Foundation for any token project.',
    category: 'token',
    features: ['SIP-010 Compliant', 'Mint/Burn', 'Transfer Controls', 'Authorized Minters', 'Max Supply'],
    fileName: 'fungible-token.clar'
  },
  {
    id: 'token-vesting',
    name: 'Token Vesting',
    description: 'Linear and cliff vesting schedules for team tokens, advisors, and investors. Revocable schedules available.',
    category: 'token',
    features: ['Cliff Vesting', 'Linear Release', 'Revocable', 'Multi-beneficiary', 'Progress Tracking'],
    fileName: 'token-vesting.clar'
  },
  {
    id: 'token-airdrop',
    name: 'Token Airdrop',
    description: 'Distribute tokens to multiple recipients with whitelist support. Perfect for community rewards and marketing.',
    category: 'token',
    features: ['Batch Distribution', 'Whitelist Support', 'Expiring Claims', 'Campaign Stats', 'Unclaimed Recovery'],
    fileName: 'token-airdrop.clar'
  },
  {
    id: 'token-swap',
    name: 'Token Swap',
    description: 'Atomic swaps between different tokens. Create swap orders with optional counterparty restrictions.',
    category: 'token',
    features: ['Atomic Swaps', 'Order Book', 'Expiring Orders', 'Counterparty Lock', '0.3% Fee'],
    fileName: 'token-swap.clar'
  },
  {
    id: 'token-bridge',
    name: 'Token Bridge',
    description: 'Bridge tokens between Stacks and other chains with oracle verification. Multi-chain support.',
    category: 'token',
    features: ['Cross-chain', 'Oracle Verification', 'Multi-chain', 'Liquidity Pool', 'Bridge Stats'],
    fileName: 'token-bridge.clar'
  },

  // DeFi Contracts
  {
    id: 'lending-protocol',
    name: 'Lending Protocol',
    description: 'Deposit collateral and borrow against it. Features liquidation, health factors, and dynamic interest rates.',
    category: 'defi',
    features: ['Collateralized Loans', 'Liquidation', 'Dynamic Rates', 'Health Factor', '75% Max LTV'],
    fileName: 'lending-protocol.clar'
  },
  {
    id: 'liquidity-pool',
    name: 'Liquidity Pool (AMM)',
    description: 'Automated Market Maker with constant product formula. Provide liquidity and earn trading fees.',
    category: 'defi',
    features: ['AMM Trading', 'LP Tokens', 'Price Discovery', '0.3% Swap Fee', 'Slippage Protection'],
    fileName: 'liquidity-pool.clar'
  },
  {
    id: 'yield-farming',
    name: 'Yield Farming',
    description: 'Stake LP tokens to earn reward tokens. Create multiple farms with different reward rates.',
    category: 'defi',
    features: ['Farm Creation', 'LP Staking', 'Reward Distribution', 'APR Calculation', 'Compound'],
    fileName: 'yield-farming.clar'
  },
  {
    id: 'staking-pool',
    name: 'Staking Pool',
    description: 'Stake tokens with flexible or locked options. Higher APY for longer lock periods.',
    category: 'defi',
    features: ['Flexible/Locked', 'Tiered APY', 'Early Unstake Fee', 'Cooldown Period', 'Reward Pool'],
    fileName: 'staking-pool.clar'
  },
  {
    id: 'flash-loan',
    name: 'Flash Loan',
    description: 'Uncollateralized loans that must be repaid within the same transaction. 0.09% fee.',
    category: 'defi',
    features: ['Instant Loans', 'No Collateral', 'Same-tx Repay', '0.09% Fee', 'Callback System'],
    fileName: 'flash-loan.clar'
  },

  // DAO Contracts
  {
    id: 'dao-governance',
    name: 'DAO Governance',
    description: 'Decentralized governance with proposal creation, voting, and execution. Delegation support.',
    category: 'dao',
    features: ['Proposals', 'Voting', 'Delegation', 'Quorum', 'Execution Delay'],
    fileName: 'dao-governance.clar'
  },
  {
    id: 'dao-treasury',
    name: 'DAO Treasury',
    description: 'Manage DAO funds with multi-sig spending requests. Budget allocations and spending limits.',
    category: 'dao',
    features: ['Multi-sig', 'Budget Allocation', 'Spending Limits', 'Quick Spend', 'Transaction History'],
    fileName: 'dao-treasury.clar'
  },
  {
    id: 'dao-voting',
    name: 'DAO Voting',
    description: 'Flexible voting mechanisms including single choice, multiple choice, ranked, and quadratic voting.',
    category: 'dao',
    features: ['Multiple Vote Types', 'Quadratic Voting', 'Poll Creation', 'Vote Weights', 'Results Finalization'],
    fileName: 'dao-voting.clar'
  },
  {
    id: 'dao-membership',
    name: 'DAO Membership',
    description: 'Tiered membership system with monthly fees, voting weights, and referral rewards.',
    category: 'dao',
    features: ['Membership Tiers', 'Monthly Fees', 'Referral System', 'Voting Weights', 'Benefits'],
    fileName: 'dao-membership.clar'
  },
  {
    id: 'dao-proposals',
    name: 'DAO Proposals',
    description: 'Advanced proposal management with templates for funding, parameters, and emergency actions.',
    category: 'dao',
    features: ['Proposal Templates', 'Funding Requests', 'Parameter Changes', 'Discussions', 'Execution'],
    fileName: 'dao-proposals.clar'
  },

  // Utility Contracts
  {
    id: 'escrow-service',
    name: 'Escrow Service',
    description: 'Secure escrow for peer-to-peer transactions with dispute resolution and arbiter support.',
    category: 'utility',
    features: ['P2P Escrow', 'Dispute Resolution', 'Arbiter System', 'Milestones', 'Reputation'],
    fileName: 'escrow-service.clar'
  },
  {
    id: 'subscription-service',
    name: 'Subscription Service',
    description: 'Recurring payments and subscription management. Create plans with different billing periods.',
    category: 'utility',
    features: ['Recurring Payments', 'Multiple Plans', 'Auto-renew', 'Creator Revenue', 'Subscriber Stats'],
    fileName: 'subscription-service.clar'
  },
  {
    id: 'crowdfunding',
    name: 'Crowdfunding',
    description: 'Create and manage crowdfunding campaigns with reward tiers and flexible funding options.',
    category: 'utility',
    features: ['Campaign Creation', 'Reward Tiers', 'Flexible Funding', 'Refunds', 'Progress Tracking'],
    fileName: 'crowdfunding.clar'
  },
  {
    id: 'lottery',
    name: 'Lottery',
    description: 'Decentralized lottery with provably fair drawings. Create lotteries with custom ticket prices.',
    category: 'utility',
    features: ['Fair Drawing', 'Custom Prizes', 'Ticket Sales', 'Winner Selection', 'Prize Claim'],
    fileName: 'lottery.clar'
  },
  {
    id: 'multisig-wallet',
    name: 'Multi-Sig Wallet',
    description: 'Secure wallet requiring multiple signatures for transactions. Add/remove signers, change threshold.',
    category: 'utility',
    features: ['Multi-signature', 'Threshold Config', 'Signer Management', 'Transaction Queue', 'Expiring Txs'],
    fileName: 'multisig-wallet.clar'
  },
];

export const categories = {
  nft: { name: 'NFT', color: 'category-nft', icon: '🖼️' },
  token: { name: 'Token', color: 'category-token', icon: '🪙' },
  defi: { name: 'DeFi', color: 'category-defi', icon: '💰' },
  dao: { name: 'DAO', color: 'category-dao', icon: '🏛️' },
  utility: { name: 'Utility', color: 'category-utility', icon: '🔧' },
};

export const getContractsByCategory = (category: string) => {
  return contracts.filter(c => c.category === category);
};

export const getContractById = (id: string) => {
  return contracts.find(c => c.id === id);
};
