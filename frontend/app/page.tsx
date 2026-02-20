'use client';

import { useState, useMemo } from 'react';
import Header from '@/components/Header';
import ContractCard from '@/components/ContractCard';
import CategoryFilter from '@/components/CategoryFilter';
import DeployModal from '@/components/DeployModal';
import Stats from '@/components/Stats';
import { contracts, Contract, categories } from '@/lib/contracts';
import { Search, Github, ExternalLink, Sparkles } from 'lucide-react';

export default function Home() {
  const [selectedCategory, setSelectedCategory] = useState<string | null>(null);
  const [searchQuery, setSearchQuery] = useState('');
  const [selectedContract, setSelectedContract] = useState<Contract | null>(null);
  const [selectedContracts, setSelectedContracts] = useState<Set<string>>(new Set());

  const filteredContracts = useMemo(() => {
    return contracts.filter(contract => {
      const matchesCategory = !selectedCategory || contract.category === selectedCategory;
      const matchesSearch = !searchQuery || 
        contract.name.toLowerCase().includes(searchQuery.toLowerCase()) ||
        contract.description.toLowerCase().includes(searchQuery.toLowerCase()) ||
        contract.features.some(f => f.toLowerCase().includes(searchQuery.toLowerCase()));
      return matchesCategory && matchesSearch;
    });
  }, [selectedCategory, searchQuery]);

  const contractCounts = useMemo(() => {
    const counts: Record<string, number> = {};
    Object.keys(categories).forEach(cat => {
      counts[cat] = contracts.filter(c => c.category === cat).length;
    });
    return counts;
  }, []);

  const toggleContractSelection = (contract: Contract) => {
    const newSelection = new Set(selectedContracts);
    if (newSelection.has(contract.id)) {
      newSelection.delete(contract.id);
    } else {
      newSelection.add(contract.id);
    }
    setSelectedContracts(newSelection);
  };

  return (
    <main className="min-h-screen">
      <Header />
      
      {/* Hero Section */}
      <section className="pt-32 pb-20 px-4 relative overflow-hidden">
        {/* Background Effects */}
        <div className="absolute inset-0 overflow-hidden">
          <div className="absolute top-1/4 left-1/4 w-96 h-96 bg-stacks-purple/20 rounded-full blur-3xl animate-pulse-slow" />
          <div className="absolute bottom-1/4 right-1/4 w-96 h-96 bg-pink-500/20 rounded-full blur-3xl animate-pulse-slow" style={{ animationDelay: '2s' }} />
        </div>

        <div className="max-w-7xl mx-auto relative">
          <div className="text-center mb-12">
            <div className="inline-flex items-center gap-2 px-4 py-2 rounded-full bg-stacks-purple/20 border border-stacks-purple/30 mb-6">
              <Sparkles size={16} className="text-stacks-purple" />
              <span className="text-sm font-medium">25 Production-Ready Contracts</span>
            </div>
            
            <h1 className="font-display text-4xl md:text-6xl lg:text-7xl font-bold mb-6">
              Deploy Smart Contracts
              <br />
              <span className="gradient-text">on Stacks Blockchain</span>
            </h1>
            
            <p className="text-gray-400 text-lg md:text-xl max-w-3xl mx-auto mb-8">
              A complete collection of NFT, DeFi, DAO, and utility smart contracts. 
              Built with Clarity 3.0, ready to deploy on mainnet or testnet.
            </p>

            <div className="flex flex-col sm:flex-row gap-4 justify-center">
              <a href="#contracts" className="btn-primary flex items-center justify-center gap-2">
                <Search size={18} />
                Browse Contracts
              </a>
              <a 
                href="https://github.com/serayd61/stacks-nft-marketplace" 
                target="_blank"
                rel="noopener noreferrer"
                className="btn-secondary flex items-center justify-center gap-2"
              >
                <Github size={18} />
                View on GitHub
                <ExternalLink size={14} />
              </a>
            </div>
          </div>

          {/* Stats */}
          <Stats />
        </div>
      </section>

      {/* Contracts Section */}
      <section id="contracts" className="py-20 px-4">
        <div className="max-w-7xl mx-auto">
          {/* Search and Filter */}
          <div className="mb-12 space-y-6">
            {/* Search */}
            <div className="max-w-xl mx-auto">
              <div className="relative">
                <Search size={20} className="absolute left-4 top-1/2 -translate-y-1/2 text-gray-400" />
                <input
                  type="text"
                  placeholder="Search contracts by name, feature, or description..."
                  value={searchQuery}
                  onChange={(e) => setSearchQuery(e.target.value)}
                  className="w-full pl-12 pr-4 py-3 rounded-xl bg-stacks-gray border border-white/10 focus:border-stacks-purple focus:outline-none transition-colors"
                />
              </div>
            </div>

            {/* Category Filter */}
            <CategoryFilter
              selectedCategory={selectedCategory}
              onSelectCategory={setSelectedCategory}
              contractCounts={contractCounts}
            />
          </div>

          {/* Selected Count */}
          {selectedContracts.size > 0 && (
            <div className="mb-6 p-4 rounded-xl bg-stacks-purple/20 border border-stacks-purple/30 flex items-center justify-between">
              <span className="font-medium">
                {selectedContracts.size} contract{selectedContracts.size > 1 ? 's' : ''} selected
              </span>
              <button
                onClick={() => setSelectedContracts(new Set())}
                className="text-sm text-stacks-purple hover:text-white transition-colors"
              >
                Clear selection
              </button>
            </div>
          )}

          {/* Contract Grid */}
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            {filteredContracts.map((contract, index) => (
              <div
                key={contract.id}
                className="animate-fade-in"
                style={{ animationDelay: `${index * 0.05}s` }}
              >
                <ContractCard
                  contract={contract}
                  onSelect={(c) => {
                    setSelectedContract(c);
                  }}
                  isSelected={selectedContracts.has(contract.id)}
                />
              </div>
            ))}
          </div>

          {/* No Results */}
          {filteredContracts.length === 0 && (
            <div className="text-center py-20">
              <div className="text-6xl mb-4">🔍</div>
              <h3 className="font-display text-xl font-semibold mb-2">No contracts found</h3>
              <p className="text-gray-400">Try adjusting your search or filter criteria</p>
            </div>
          )}
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-20 px-4">
        <div className="max-w-4xl mx-auto">
          <div className="gradient-border p-8 md:p-12 text-center">
            <h2 className="font-display text-3xl md:text-4xl font-bold mb-4">
              Ready to Build on Stacks?
            </h2>
            <p className="text-gray-400 text-lg mb-8 max-w-2xl mx-auto">
              Clone the repository, customize the contracts for your needs, and deploy to 
              Stacks blockchain. Full documentation and tests included.
            </p>
            <div className="flex flex-col sm:flex-row gap-4 justify-center">
              <a 
                href="https://github.com/serayd61/stacks-nft-marketplace"
                target="_blank"
                rel="noopener noreferrer"
                className="btn-primary flex items-center justify-center gap-2"
              >
                <Github size={18} />
                Clone Repository
              </a>
              <a 
                href="https://docs.stacks.co"
                target="_blank"
                rel="noopener noreferrer"
                className="btn-secondary flex items-center justify-center gap-2"
              >
                Stacks Documentation
                <ExternalLink size={14} />
              </a>
            </div>
          </div>
        </div>
      </section>

      {/* Footer */}
      <footer className="py-8 px-4 border-t border-white/10">
        <div className="max-w-7xl mx-auto flex flex-col md:flex-row items-center justify-between gap-4">
          <div className="flex items-center gap-3">
            <div className="w-8 h-8 rounded-lg bg-gradient-to-br from-stacks-purple to-pink-500 flex items-center justify-center">
              <span className="text-sm">⚡</span>
            </div>
            <span className="text-gray-400">Stacks Contract Deployer</span>
          </div>
          <div className="flex items-center gap-6 text-sm text-gray-400">
            <a href="https://github.com/serayd61/stacks-nft-marketplace" target="_blank" rel="noopener noreferrer" className="hover:text-white transition-colors">
              GitHub
            </a>
            <a href="https://docs.stacks.co" target="_blank" rel="noopener noreferrer" className="hover:text-white transition-colors">
              Docs
            </a>
            <span>Built with Clarity 3.0</span>
          </div>
        </div>
      </footer>

      {/* Deploy Modal */}
      {selectedContract && (
        <DeployModal
          contract={selectedContract}
          onClose={() => setSelectedContract(null)}
        />
      )}
    </main>
  );
}
