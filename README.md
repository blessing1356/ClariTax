# ClariTax Smart Contract

A comprehensive FIFO-based capital gains tax calculation and tracking system built on the Stacks blockchain.

## Features

- 📊 FIFO (First In, First Out) lot tracking system
- 💰 Automated capital gains calculations
- ⏱️ Short-term and long-term gain classification
- 🔄 Support for multiple assets per user
- 🏦 Built-in treasury management
- 📝 Complete transaction history
- 🛠️ Manual adjustment capability for operators
- ⚡ Gas-optimized data structures

## Technical Overview

### Constants

```clarity
ZERO (u0)
ONE (u1)
ERR_UNAUTHORIZED (u100)
...
```

### Core Functions

- **Buy**: Record asset purchases with cost basis
- **Sell**: Execute sales with automated FIFO lot matching
- **Transfer In/Out**: Support for external transfers
- **Manual Adjustments**: Operator tools for corrections

### Security Features

- Role-based access control (Admin/Operator)
- Circuit breaker (pause mechanism)
- Input validation
- Overflow protection
- Safe arithmetic operations

## Usage

### Basic Operations

```clarity
;; Buy an asset
(contract-call? .claritax buy "BTC" u100 u50000)

;; Sell an asset
(contract-call? .claritax sell "BTC" u50 u55000)

;; Check portfolio
(contract-call? .claritax portfolio-summary tx-sender "BTC" u54000)
```

### Administrative Functions

```clarity
;; Set tax rate (requires admin/operator)
(contract-call? .claritax set-tax-rate-bps u1500)

;; Set long-term holding period
(contract-call? .claritax set-lt-hold-blocks u105120)
```

## Configuration

- Default tax rate: 15.00% (1500 basis points)
- Long-term holding period: ~1 year (105,120 blocks)
- Default withholding: Disabled

## Installation

1. Clone the repository
2. Deploy using Clarinet or Stacks CLI
3. Initialize admin controls
4. Configure tax parameters

## Testing

Comprehensive test suite available in the tests directory covering:
- Lot management
- Tax calculations
- Access controls
- Error conditions



tact maintainers




Built with ❤️ for the Stacks ecosystem
