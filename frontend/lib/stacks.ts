'use client';

import { AppConfig, UserSession, showConnect } from '@stacks/connect';

const appConfig = new AppConfig(['store_write', 'publish_data']);
export const userSession = new UserSession({ appConfig });

export const appDetails = {
  name: 'Stacks Contract Deployer',
  icon: '/logo.png',
};

export const connectWallet = () => {
  showConnect({
    appDetails,
    redirectTo: '/',
    onFinish: () => {
      window.location.reload();
    },
    userSession,
  });
};

export const disconnectWallet = () => {
  userSession.signUserOut('/');
};

export const isUserSignedIn = () => {
  if (typeof window === 'undefined') return false;
  return userSession.isUserSignedIn();
};

export const getUserData = () => {
  if (typeof window === 'undefined') return null;
  if (isUserSignedIn()) {
    return userSession.loadUserData();
  }
  return null;
};

export const getStxAddress = () => {
  const userData = getUserData();
  if (userData) {
    const network = process.env.NEXT_PUBLIC_NETWORK || 'testnet';
    return network === 'mainnet' 
      ? userData.profile.stxAddress.mainnet 
      : userData.profile.stxAddress.testnet;
  }
  return null;
};

export const getNetwork = () => {
  const network = process.env.NEXT_PUBLIC_NETWORK || 'testnet';
  return network;
};

export const truncateAddress = (address: string) => {
  if (!address) return '';
  return `${address.slice(0, 6)}...${address.slice(-4)}`;
};
