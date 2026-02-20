import { AppConfig, UserSession, showConnect } from '@stacks/connect';
import { StacksMainnet, StacksTestnet } from '@stacks/network';

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
  return userSession.isUserSignedIn();
};

export const getUserData = () => {
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
  return network === 'mainnet' ? new StacksMainnet() : new StacksTestnet();
};

export const truncateAddress = (address: string) => {
  if (!address) return '';
  return `${address.slice(0, 6)}...${address.slice(-4)}`;
};
