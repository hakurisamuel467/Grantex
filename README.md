# 🧪 Grantex - Research Grant Tracker

A smart contract system for managing research grants with milestone-based fund releases on the Stacks blockchain.

## 📋 Overview

Grantex enables transparent and automated management of research grants through:
- 🎯 Milestone-based fund distribution
- 👥 Multi-reviewer authorization system  
- 📊 Real-time progress tracking
- 🔒 Secure fund management

## ✨ Features

- **Grant Creation**: Create grants with custom milestones and funding amounts
- **Milestone Tracking**: Monitor research progress through defined milestones
- **Automated Releases**: Funds automatically released upon milestone completion
- **Reviewer System**: Authorized reviewers can approve milestone completions
- **Progress Analytics**: Real-time tracking of grant progress and fund distribution

## 🚀 Usage

### For Grant Administrators

#### Create a New Grant
```clarity
(contract-call? .Grantex create-grant 
  'ST1RESEARCHER123 
  "AI Research Project" 
  u1000000 
  (list "Literature Review" "Prototype Development" "Final Report")
  (list u300000 u500000 u200000))
```

#### Authorize Reviewers
```clarity
(contract-call? .Grantex authorize-reviewer 'ST1REVIEWER123)
```

#### Fund the Contract
```clarity
(contract-call? .Grantex fund-contract u5000000)
```

### For Reviewers

#### Complete a Milestone
```clarity
(contract-call? .Grantex complete-milestone u1 u1)
```

### Read-Only Functions

#### Get Grant Details
```clarity
(contract-call? .Grantex get-grant u1)
```

#### Check Grant Progress
```clarity
(contract-call? .Grantex get-grant-progress u1)
```

#### View Milestone Status
```clarity
(contract-call? .Grantex get-milestone u1 u1)
```

## 📊 Data Structure

### Grant Object
- `researcher`: Principal address of the researcher
- `title`: Grant title (max 100 characters)
- `total-amount`: Total grant amount in microSTX
- `released-amount`: Amount already released
- `created-at`: Block height when created
- `status`: Current grant status

### Milestone Object
- `description`: Milestone description (max 200 characters)
- `amount`: Funding amount for this milestone
- `completed`: Completion status
- `completed-at`: Block height when completed
- `reviewer`: Principal who approved completion

## 🔐 Access Control

- **Contract Owner**: Can create grants, authorize reviewers, update grant status
- **Authorized Reviewers**: Can complete milestones and trigger fund releases
- **Researchers**: Receive funds automatically upon milestone completion

## 🛠️ Development

### Prerequisites
- Clarinet CLI installed
- Stacks wallet for testing

### Testing
```bash
clarinet test
```

### Deployment
```bash
clarinet deploy
```

## 📈 Error Codes

- `u100`: Unauthorized access
- `u101`: Grant not found
- `u102`: Milestone not found  
- `u103`: Insufficient funds
- `u104`: Milestone already completed
- `u105`: Invalid milestone configuration
- `u106`: Grant already exists
- `u107`: Invalid amount
- `u108`: Milestone not ready

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

## 📄 License

