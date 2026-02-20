import { describe, it, expect, beforeEach } from "vitest";
import { Cl, ClarityType } from "@stacks/transactions";
import { initSimnet } from "@hirosystems/clarinet-sdk";

// Initialize simnet for contract testing
const simnet = await initSimnet();
const accounts = simnet.getAccounts();

const deployer = accounts.get("deployer")!;
const wallet1 = accounts.get("wallet_1")!;
const wallet2 = accounts.get("wallet_2")!;

const CONTRACT_NAME = "nft-marketplace";

describe("NFT Marketplace - Fixed Price Listings", () => {
  it("should list NFT for sale with valid price", () => {
    const result = simnet.callPublicFn(
      CONTRACT_NAME,
      "list-nft",
      [
        Cl.principal(deployer), // nft-contract (mock)
        Cl.uint(1),             // token-id
        Cl.uint(100_000_000),   // price: 100 STX
        Cl.uint(10080),         // duration: ~7 days in blocks
      ],
      wallet1
    );

    expect(result.result.type).toBe(ClarityType.ResponseOk);
    const okVal = result.result as { value: { data: { "listing-id": { value: bigint } } } };
    expect(okVal.value.data["listing-id"].value).toBe(0n);
  });

  it("should reject listing with zero price", () => {
    const result = simnet.callPublicFn(
      CONTRACT_NAME,
      "list-nft",
      [
        Cl.principal(deployer),
        Cl.uint(1),
        Cl.uint(0), // invalid price
        Cl.uint(10080),
      ],
      wallet1
    );

    expect(result.result.type).toBe(ClarityType.ResponseErr);
  });

  it("should retrieve listing details after listing", () => {
    // First create a listing
    simnet.callPublicFn(
      CONTRACT_NAME,
      "list-nft",
      [Cl.principal(deployer), Cl.uint(2), Cl.uint(50_000_000), Cl.uint(5000)],
      wallet1
    );

    // Then retrieve it
    const listing = simnet.callReadOnlyFn(
      CONTRACT_NAME,
      "get-listing",
      [Cl.uint(0)],
      wallet1
    );

    expect(listing.result.type).toBe(ClarityType.OptionalSome);
  });

  it("should allow seller to cancel their own listing", () => {
    // Create listing
    simnet.callPublicFn(
      CONTRACT_NAME,
      "list-nft",
      [Cl.principal(deployer), Cl.uint(3), Cl.uint(75_000_000), Cl.uint(5000)],
      wallet1
    );

    // Cancel it
    const cancel = simnet.callPublicFn(
      CONTRACT_NAME,
      "cancel-listing",
      [Cl.uint(0)],
      wallet1
    );

    expect(cancel.result.type).toBe(ClarityType.ResponseOk);
  });

  it("should reject cancel from non-owner", () => {
    // wallet1 creates listing
    simnet.callPublicFn(
      CONTRACT_NAME,
      "list-nft",
      [Cl.principal(deployer), Cl.uint(4), Cl.uint(75_000_000), Cl.uint(5000)],
      wallet1
    );

    // wallet2 tries to cancel — should fail
    const cancel = simnet.callPublicFn(
      CONTRACT_NAME,
      "cancel-listing",
      [Cl.uint(0)],
      wallet2
    );

    expect(cancel.result.type).toBe(ClarityType.ResponseErr);
  });

  it("should allow seller to update listing price", () => {
    simnet.callPublicFn(
      CONTRACT_NAME,
      "list-nft",
      [Cl.principal(deployer), Cl.uint(5), Cl.uint(50_000_000), Cl.uint(5000)],
      wallet1
    );

    const update = simnet.callPublicFn(
      CONTRACT_NAME,
      "update-price",
      [Cl.uint(0), Cl.uint(80_000_000)],
      wallet1
    );

    expect(update.result.type).toBe(ClarityType.ResponseOk);
  });
});

describe("NFT Marketplace - Auctions", () => {
  it("should create auction with valid parameters", () => {
    const result = simnet.callPublicFn(
      CONTRACT_NAME,
      "create-auction",
      [
        Cl.principal(deployer),  // nft-contract
        Cl.uint(10),             // token-id
        Cl.uint(10_000_000),     // start-price: 10 STX
        Cl.uint(50_000_000),     // reserve-price: 50 STX
        Cl.uint(4320),           // duration: ~3 days
      ],
      wallet1
    );

    expect(result.result.type).toBe(ClarityType.ResponseOk);
  });

  it("should reject auction where reserve < start price", () => {
    const result = simnet.callPublicFn(
      CONTRACT_NAME,
      "create-auction",
      [
        Cl.principal(deployer),
        Cl.uint(11),
        Cl.uint(50_000_000),  // start price higher than reserve
        Cl.uint(10_000_000),  // invalid: reserve < start
        Cl.uint(4320),
      ],
      wallet1
    );

    expect(result.result.type).toBe(ClarityType.ResponseErr);
  });

  it("should accept valid bid on active auction", () => {
    // Create auction first
    simnet.callPublicFn(
      CONTRACT_NAME,
      "create-auction",
      [Cl.principal(deployer), Cl.uint(12), Cl.uint(10_000_000), Cl.uint(50_000_000), Cl.uint(4320)],
      wallet1
    );

    // wallet2 places bid
    const bid = simnet.callPublicFn(
      CONTRACT_NAME,
      "place-bid",
      [Cl.uint(0), Cl.uint(15_000_000)], // 15 STX bid
      wallet2
    );

    expect(bid.result.type).toBe(ClarityType.ResponseOk);
  });

  it("should reject bid below minimum increment", () => {
    simnet.callPublicFn(
      CONTRACT_NAME,
      "create-auction",
      [Cl.principal(deployer), Cl.uint(13), Cl.uint(10_000_000), Cl.uint(50_000_000), Cl.uint(4320)],
      wallet1
    );

    // First valid bid
    simnet.callPublicFn(
      CONTRACT_NAME,
      "place-bid",
      [Cl.uint(0), Cl.uint(15_000_000)],
      wallet2
    );

    // Second bid too low (must be current + 1 STX minimum)
    const lowBid = simnet.callPublicFn(
      CONTRACT_NAME,
      "place-bid",
      [Cl.uint(0), Cl.uint(15_000_500)], // less than 1 STX increment
      wallet1
    );

    expect(lowBid.result.type).toBe(ClarityType.ResponseErr);
  });
});

describe("NFT Marketplace - Fee Calculation", () => {
  it("should calculate 2.5% platform fee correctly for 100 STX", () => {
    const result = simnet.callReadOnlyFn(
      CONTRACT_NAME,
      "calculate-fee",
      [Cl.uint(100_000_000)], // 100 STX in microSTX
      deployer
    );

    // 2.5% of 100 STX = 2.5 STX = 2,500,000 microSTX
    expect(result.result).toEqual(Cl.uint(2_500_000));
  });

  it("should calculate fee correctly for 1 STX", () => {
    const result = simnet.callReadOnlyFn(
      CONTRACT_NAME,
      "calculate-fee",
      [Cl.uint(1_000_000)], // 1 STX
      deployer
    );

    // 2.5% of 1 STX = 25,000 microSTX
    expect(result.result).toEqual(Cl.uint(25_000));
  });

  it("should return zero fee for zero price", () => {
    const result = simnet.callReadOnlyFn(
      CONTRACT_NAME,
      "calculate-fee",
      [Cl.uint(0)],
      deployer
    );

    expect(result.result).toEqual(Cl.uint(0));
  });
});

describe("NFT Marketplace - Stats & Read-Only", () => {
  it("should return default seller stats for new user", () => {
    const stats = simnet.callReadOnlyFn(
      CONTRACT_NAME,
      "get-seller-stats",
      [Cl.principal(wallet1)],
      deployer
    );

    expect(stats.result.type).toBe(ClarityType.Tuple);
  });

  it("should return default buyer stats for new user", () => {
    const stats = simnet.callReadOnlyFn(
      CONTRACT_NAME,
      "get-buyer-stats",
      [Cl.principal(wallet2)],
      deployer
    );

    expect(stats.result.type).toBe(ClarityType.Tuple);
  });

  it("should return marketplace stats with correct structure", () => {
    const stats = simnet.callReadOnlyFn(
      CONTRACT_NAME,
      "get-marketplace-stats",
      [],
      deployer
    );

    expect(stats.result.type).toBe(ClarityType.Tuple);
  });

  it("should report listing as inactive after cancellation", () => {
    simnet.callPublicFn(
      CONTRACT_NAME,
      "list-nft",
      [Cl.principal(deployer), Cl.uint(20), Cl.uint(50_000_000), Cl.uint(5000)],
      wallet1
    );

    simnet.callPublicFn(CONTRACT_NAME, "cancel-listing", [Cl.uint(0)], wallet1);

    const isActive = simnet.callReadOnlyFn(
      CONTRACT_NAME,
      "is-listing-active",
      [Cl.uint(0)],
      deployer
    );

    expect(isActive.result).toEqual(Cl.bool(false));
  });

  it("should return none for non-existent listing", () => {
    const listing = simnet.callReadOnlyFn(
      CONTRACT_NAME,
      "get-listing",
      [Cl.uint(9999)],
      deployer
    );

    expect(listing.result.type).toBe(ClarityType.OptionalNone);
  });
});
