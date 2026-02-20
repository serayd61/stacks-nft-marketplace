'use client';

import { useState } from 'react';
import { Contract, categories } from '@/lib/contracts';
import { X, Rocket, Code, AlertCircle, ExternalLink, Copy, Check } from 'lucide-react';

interface DeployModalProps {
  contract: Contract;
  onClose: () => void;
}

export default function DeployModal({ contract, onClose }: DeployModalProps) {
  const [copied, setCopied] = useState(false);
  const category = categories[contract.category];

  const clarinetCommand = `clarinet contract deploy ${contract.id}`;
  const githubUrl = `https://github.com/serayd61/stacks-nft-marketplace/blob/main/contracts/${contract.fileName}`;

  const copyCommand = () => {
    navigator.clipboard.writeText(clarinetCommand);
    setCopied(true);
    setTimeout(() => setCopied(false), 2000);
  };

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center p-4">
      {/* Backdrop */}
      <div 
        className="absolute inset-0 bg-black/80 backdrop-blur-sm"
        onClick={onClose}
      />
      
      {/* Modal */}
      <div className="relative w-full max-w-2xl glass-card p-6 animate-fade-in">
        {/* Close Button */}
        <button
          onClick={onClose}
          className="absolute top-4 right-4 p-2 rounded-lg hover:bg-white/10 transition-colors"
        >
          <X size={20} />
        </button>

        {/* Header */}
        <div className="flex items-start gap-4 mb-6">
          <div className="w-14 h-14 rounded-xl bg-gradient-to-br from-stacks-purple to-pink-500 flex items-center justify-center text-2xl">
            {category.icon}
          </div>
          <div>
            <h2 className="font-display font-bold text-2xl">{contract.name}</h2>
            <span className={`category-badge ${category.color} mt-2`}>
              {category.name}
            </span>
          </div>
        </div>

        {/* Description */}
        <p className="text-gray-300 mb-6">
          {contract.description}
        </p>

        {/* Features */}
        <div className="mb-6">
          <h3 className="font-semibold mb-3 flex items-center gap-2">
            <Code size={18} />
            Features
          </h3>
          <div className="flex flex-wrap gap-2">
            {contract.features.map((feature, index) => (
              <span
                key={index}
                className="px-3 py-1.5 text-sm rounded-lg bg-stacks-purple/20 text-stacks-purple border border-stacks-purple/30"
              >
                {feature}
              </span>
            ))}
          </div>
        </div>

        {/* Deployment Instructions */}
        <div className="mb-6 p-4 rounded-xl bg-stacks-gray border border-white/10">
          <h3 className="font-semibold mb-3 flex items-center gap-2">
            <Rocket size={18} />
            Deploy with Clarinet
          </h3>
          <div className="flex items-center gap-2 p-3 rounded-lg bg-black/50 font-mono text-sm">
            <code className="flex-1 text-green-400">{clarinetCommand}</code>
            <button
              onClick={copyCommand}
              className="p-2 rounded-lg hover:bg-white/10 transition-colors"
            >
              {copied ? <Check size={16} className="text-green-400" /> : <Copy size={16} />}
            </button>
          </div>
        </div>

        {/* Warning */}
        <div className="mb-6 p-4 rounded-xl bg-yellow-500/10 border border-yellow-500/30">
          <div className="flex items-start gap-3">
            <AlertCircle size={20} className="text-yellow-500 flex-shrink-0 mt-0.5" />
            <div>
              <h4 className="font-semibold text-yellow-500 mb-1">Before Deploying</h4>
              <p className="text-sm text-gray-300">
                Review the contract code carefully. Modify parameters like fees, addresses, and limits 
                according to your needs. Test on testnet before deploying to mainnet.
              </p>
            </div>
          </div>
        </div>

        {/* Actions */}
        <div className="flex flex-col sm:flex-row gap-3">
          <a
            href={githubUrl}
            target="_blank"
            rel="noopener noreferrer"
            className="btn-primary flex items-center justify-center gap-2 flex-1"
          >
            <Code size={18} />
            View Source Code
            <ExternalLink size={14} />
          </a>
          <a
            href="https://docs.stacks.co/clarity/clarity-smart-contracts"
            target="_blank"
            rel="noopener noreferrer"
            className="btn-secondary flex items-center justify-center gap-2 flex-1"
          >
            Clarity Docs
            <ExternalLink size={14} />
          </a>
        </div>
      </div>
    </div>
  );
}
