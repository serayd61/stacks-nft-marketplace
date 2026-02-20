'use client';

import { Contract, categories } from '@/lib/contracts';
import { Code, ExternalLink, Check } from 'lucide-react';

interface ContractCardProps {
  contract: Contract;
  onSelect: (contract: Contract) => void;
  isSelected: boolean;
}

export default function ContractCard({ contract, onSelect, isSelected }: ContractCardProps) {
  const category = categories[contract.category];

  return (
    <div
      className={`glass-card p-6 hover-lift cursor-pointer transition-all duration-300 ${
        isSelected ? 'ring-2 ring-stacks-purple' : ''
      }`}
      onClick={() => onSelect(contract)}
    >
      {/* Header */}
      <div className="flex items-start justify-between mb-4">
        <div className="flex items-center gap-3">
          <div className="text-2xl">{category.icon}</div>
          <div>
            <h3 className="font-display font-semibold text-lg">{contract.name}</h3>
            <span className={`category-badge ${category.color}`}>
              {category.name}
            </span>
          </div>
        </div>
        <div className={`w-6 h-6 rounded-full flex items-center justify-center transition-all ${
          isSelected ? 'bg-stacks-purple' : 'bg-stacks-gray border border-white/20'
        }`}>
          {isSelected && <Check size={14} />}
        </div>
      </div>

      {/* Description */}
      <p className="text-gray-400 text-sm mb-4 line-clamp-2">
        {contract.description}
      </p>

      {/* Features */}
      <div className="flex flex-wrap gap-2 mb-4">
        {contract.features.slice(0, 3).map((feature, index) => (
          <span
            key={index}
            className="px-2 py-1 text-xs rounded-md bg-white/5 text-gray-300"
          >
            {feature}
          </span>
        ))}
        {contract.features.length > 3 && (
          <span className="px-2 py-1 text-xs rounded-md bg-white/5 text-gray-400">
            +{contract.features.length - 3} more
          </span>
        )}
      </div>

      {/* Footer */}
      <div className="flex items-center justify-between pt-4 border-t border-white/10">
        <div className="flex items-center gap-2 text-gray-400 text-sm">
          <Code size={14} />
          <span>{contract.fileName}</span>
        </div>
        <a
          href={`https://github.com/serayd61/stacks-nft-marketplace/blob/main/contracts/${contract.fileName}`}
          target="_blank"
          rel="noopener noreferrer"
          className="text-stacks-purple hover:text-white transition-colors"
          onClick={(e) => e.stopPropagation()}
        >
          <ExternalLink size={16} />
        </a>
      </div>
    </div>
  );
}
