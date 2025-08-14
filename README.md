# Secure DAO Voting with Privacy Layer

This Clarity smart contract implements a secure voting system for DAOs with a privacy layer that allows for anonymous voting using zero-knowledge proofs.

## Features

- **Private Voting**: Uses commitment scheme and zero-knowledge proofs to keep votes private
- **Proposal Management**: Create, vote on, and finalize proposals
- **Voting Power**: Weighted voting based on token holdings
- **Admin Controls**: DAO admin can manage voting parameters

## Contract Overview

The contract implements the following core functionality:

1. **Proposal Creation**: Any user with sufficient voting power can create a proposal
2. **Anonymous Voting**: Users can vote on proposals without revealing their vote choice
3. **Proposal Finalization**: After the voting period ends, proposals can be finalized
4. **Execution**: Passed proposals can be executed by the DAO admin

## How It Works

### Privacy Mechanism

The privacy layer works through a combination of on-chain and off-chain components:

1. **Off-chain**: Users generate a commitment to their vote and a zero-knowledge proof
2. **On-chain**: The contract verifies the proof and records the commitment
3. **Merkle Tree**: A Merkle tree is used to efficiently verify vote eligibility

### Usage Flow

1. **Setup**: DAO admin assigns voting power to members
2. **Proposal Creation**: Members create proposals with a voting period
3. **Voting**: Members generate proofs off-chain and submit votes on-chain
4. **Finalization**: After the voting period, anyone can finalize the proposal
5. **Execution**: DAO admin executes passed proposals

## Contract Functions

### Admin Functions

- `set-dao-admin`: Change the DAO admin
- `set-min-voting-power`: Set minimum voting power required to participate
- `add-voting-power`: Assign voting power to a user
- `execute-proposal`: Execute a passed proposal

### User Functions

- `create-proposal`: Create a new proposal
- `cast-vote`: Vote on a proposal with a zero-knowledge proof
- `finalize-proposal`: Finalize a proposal after voting ends

### Read-only Functions

- `get-dao-admin`: Get the current DAO admin
- `get-proposal-count`: Get the total number of proposals
- `get-proposal`: Get details of a specific proposal
- `get-vote-receipt`: Check if a user has voted on a proposal
- `get-voting-power`: Get a user's voting power
- `is-proposal-active`: Check if a proposal is currently active
- `is-proposal-ended`: Check if a proposal's voting period has ended

## Integration with Off-chain Components

To fully implement the privacy layer, this contract should be paired with:

1. An off-chain proof generator for creating zero-knowledge proofs
2. A client application that handles key management and proof generation
3. A verification system that can validate the proofs on-chain

## Security Considerations

- The contract uses a simplified proof verification system in this MVP
- In production, a proper zk-SNARK or zk-STARK verification system should be implemented
- The Merkle root should be generated from an off-chain component that tracks eligible voters

