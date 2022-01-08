import 'katex/dist/katex.css';
import '../styles/globals.scss';

import type { AppProps } from 'next/app';

const App = ({ Component, pageProps }: AppProps): React.ReactNode => {
    return <Component {...pageProps} />;
};

export default App;
