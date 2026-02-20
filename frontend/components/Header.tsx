'use client';

import { useState, useEffect } from 'react';
import { connectWallet, disconnectWallet, isUserSignedIn, getStxAddress, truncateAddress } from '@/lib/stacks';
import { Wallet, LogOut, Menu, X } from 'lucide-react';

export default function Header() {
  const [isConnected, setIsConnected] = useState(false);
  const [address, setAddress] = useState<string | null>(null);
  const [mobileMenuOpen, setMobileMenuOpen] = useState(false);

  useEffect(() => {
    const connected = isUserSignedIn();
    setIsConnected(connected);
    if (connected) {
      setAddress(getStxAddress());
    }
  }, []);

  return (
    <header className="fixed top-0 left-0 right-0 z-50 glass-card border-b border-white/10">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex items-center justify-between h-16">
          {/* Logo */}
          <div className="flex items-center gap-3">
            <div className="w-10 h-10 rounded-xl bg-gradient-to-br from-stacks-purple to-pink-500 flex items-center justify-center">
              <span className="text-xl">⚡</span>
            </div>
            <div>
              <h1 className="font-display font-bold text-lg">Stacks Deployer</h1>
              <p className="text-xs text-gray-400">25 Smart Contracts</p>
            </div>
          </div>

          {/* Desktop Navigation */}
          <nav className="hidden md:flex items-center gap-6">
            <a href="#contracts" className="text-gray-300 hover:text-white transition-colors">
              Contracts
            </a>
            <a href="#categories" className="text-gray-300 hover:text-white transition-colors">
              Categories
            </a>
            <a href="https://github.com/serayd61/stacks-nft-marketplace" target="_blank" rel="noopener noreferrer" className="text-gray-300 hover:text-white transition-colors">
              GitHub
            </a>
          </nav>

          {/* Wallet Button */}
          <div className="flex items-center gap-4">
            {isConnected ? (
              <div className="flex items-center gap-3">
                <div className="hidden sm:flex items-center gap-2 px-4 py-2 rounded-xl bg-stacks-gray border border-white/10">
                  <div className="w-2 h-2 rounded-full bg-green-400 animate-pulse" />
                  <span className="text-sm font-medium">{truncateAddress(address || '')}</span>
                </div>
                <button
                  onClick={disconnectWallet}
                  className="p-2 rounded-xl bg-red-500/20 text-red-400 hover:bg-red-500/30 transition-colors"
                >
                  <LogOut size={20} />
                </button>
              </div>
            ) : (
              <button
                onClick={connectWallet}
                className="btn-primary flex items-center gap-2"
              >
                <Wallet size={18} />
                <span className="hidden sm:inline">Connect Wallet</span>
              </button>
            )}

            {/* Mobile Menu Button */}
            <button
              onClick={() => setMobileMenuOpen(!mobileMenuOpen)}
              className="md:hidden p-2 rounded-xl bg-stacks-gray"
            >
              {mobileMenuOpen ? <X size={20} /> : <Menu size={20} />}
            </button>
          </div>
        </div>
      </div>

      {/* Mobile Menu */}
      {mobileMenuOpen && (
        <div className="md:hidden border-t border-white/10 bg-stacks-dark">
          <nav className="flex flex-col p-4 gap-4">
            <a href="#contracts" className="text-gray-300 hover:text-white transition-colors py-2">
              Contracts
            </a>
            <a href="#categories" className="text-gray-300 hover:text-white transition-colors py-2">
              Categories
            </a>
            <a href="https://github.com/serayd61/stacks-nft-marketplace" target="_blank" rel="noopener noreferrer" className="text-gray-300 hover:text-white transition-colors py-2">
              GitHub
            </a>
          </nav>
        </div>
      )}
    </header>
  );
}
