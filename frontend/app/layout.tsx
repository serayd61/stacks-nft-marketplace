import type { Metadata } from 'next'
import './globals.css'

export const metadata: Metadata = {
  title: 'Stacks Contract Deployer | Deploy Smart Contracts on Stacks',
  description: 'Deploy and manage 25+ smart contracts on Stacks blockchain. NFT, DeFi, DAO, and utility contracts ready to deploy.',
  keywords: ['Stacks', 'Smart Contracts', 'Clarity', 'NFT', 'DeFi', 'DAO', 'Blockchain'],
}

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="en">
      <body>
        <div className="noise-overlay" />
        {children}
      </body>
    </html>
  )
}
