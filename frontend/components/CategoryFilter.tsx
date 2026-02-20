'use client';

import { categories } from '@/lib/contracts';

interface CategoryFilterProps {
  selectedCategory: string | null;
  onSelectCategory: (category: string | null) => void;
  contractCounts: Record<string, number>;
}

export default function CategoryFilter({ 
  selectedCategory, 
  onSelectCategory,
  contractCounts 
}: CategoryFilterProps) {
  return (
    <div className="flex flex-wrap gap-3 justify-center" id="categories">
      <button
        onClick={() => onSelectCategory(null)}
        className={`px-4 py-2 rounded-xl font-medium transition-all ${
          selectedCategory === null
            ? 'bg-stacks-purple text-white'
            : 'bg-stacks-gray text-gray-300 hover:bg-white/10'
        }`}
      >
        All ({Object.values(contractCounts).reduce((a, b) => a + b, 0)})
      </button>
      
      {Object.entries(categories).map(([key, category]) => (
        <button
          key={key}
          onClick={() => onSelectCategory(key)}
          className={`px-4 py-2 rounded-xl font-medium transition-all flex items-center gap-2 ${
            selectedCategory === key
              ? 'bg-stacks-purple text-white'
              : 'bg-stacks-gray text-gray-300 hover:bg-white/10'
          }`}
        >
          <span>{category.icon}</span>
          <span>{category.name}</span>
          <span className="text-xs opacity-70">({contractCounts[key] || 0})</span>
        </button>
      ))}
    </div>
  );
}
