'use client';

let AppConfig: any;
let UserSession: any;
let showConnect: any;
let userSession: any;

if (typeof window !== 'undefined') {
  const connect = require('@stacks/connect');
  AppConfig = connect.AppConfig;
  UserSession = connect.UserSession;
  showConnect = connect.showConnect;
  
  const appConfig = new AppConfig(['store_write', 'publish_data']);
  userSession = new UserSession({ appConfig });
}

export const appDetails = {
  name: 'Stacks Contract Deployer',
  icon: typeof window !== 'undefined' ? `${window.location.origin}/logo.png` : '/logo.png',
};

export const connectWallet = () => {
  if (typeof window === 'undefined' || !showConnect || !userSession) {
    console.error('Stacks Connect not available');
    return;
  }
  
  try {
    showConnect({
      appDetails,
      redirectTo: '/',
      onFinish: () => {
        window.location.reload();
      },
      onCancel: () => {
        console.log('User cancelled');
      },
      userSession,
    });
  } catch (error) {
    console.error('Connect error:', error);
  }
};

export const disconnectWallet = () => {
  if (userSession) {
    userSession.signUserOut('/');
  }
};

export const isUserSignedIn = (): boolean => {
  if (typeof window === 'undefined' || !userSession) return false;
  try {
    return userSession.isUserSignedIn();
  } catch {
    return false;
  }
};

export const getUserData = () => {
  if (typeof window === 'undefined' || !userSession) return null;
  try {
    if (isUserSignedIn()) {
      return userSession.loadUserData();
    }
  } catch {
    return null;
  }
  return null;
};

export const getStxAddress = (): string | null => {
  const userData = getUserData();
  if (userData) {
    const network = process.env.NEXT_PUBLIC_NETWORK || 'testnet';
    try {
      return network === 'mainnet' 
        ? userData.profile.stxAddress.mainnet 
        : userData.profile.stxAddress.testnet;
    } catch {
      return null;
    }
  }
  return null;
};

export const truncateAddress = (address: string): string => {
  if (!address) return '';
  return `${address.slice(0, 6)}...${address.slice(-4)}`;
};
