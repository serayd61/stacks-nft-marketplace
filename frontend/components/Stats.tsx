'use client';

import { contracts, categories } from '@/lib/contracts';
import { FileCode, Layers, Shield, Zap } from 'lucide-react';

export default function Stats() {
  const stats = [
    {
      icon: <FileCode size={24} />,
      value: contracts.length,
      label: 'Smart Contracts',
      color: 'from-stacks-purple to-blue-500',
    },
    {
      icon: <Layers size={24} />,
      value: Object.keys(categories).length,
      label: 'Categories',
      color: 'from-pink-500 to-rose-500',
    },
    {
      icon: <Shield size={24} />,
      value: 'Clarity 3.0',
      label: 'Version',
      color: 'from-green-500 to-emerald-500',
    },
    {
      icon: <Zap size={24} />,
      value: 'Production',
      label: 'Ready',
      color: 'from-yellow-500 to-orange-500',
    },
  ];

  return (
    <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
      {stats.map((stat, index) => (
        <div
          key={index}
          className="glass-card p-6 text-center hover-lift animate-fade-in"
          style={{ animationDelay: `${index * 0.1}s` }}
        >
          <div className={`w-12 h-12 mx-auto mb-3 rounded-xl bg-gradient-to-br ${stat.color} flex items-center justify-center`}>
            {stat.icon}
          </div>
          <div className="font-display font-bold text-2xl mb-1">{stat.value}</div>
          <div className="text-gray-400 text-sm">{stat.label}</div>
        </div>
      ))}
    </div>
  );
}
