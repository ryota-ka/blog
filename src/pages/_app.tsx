import 'katex/dist/katex.css';

import '../styles/globals.scss';
import '../styles/hljs.scss';

import mermaid from 'mermaid';
import type { AppProps } from 'next/app';
import Script from 'next/script';
import { useEffect } from 'react';

declare global {
    // eslint-disable-next-line no-var
    var gtag: any;
    // eslint-disable-next-line no-var
    var twttr: {
        widgets: {
            createTweet: (id: string, container: HTMLElement) => void;
        };
    };
}

const gaTrackingID = process.env.NEXT_PUBLIC_GOOGLE_ANALYTICS_TRACKING_ID!;

const App = ({ Component, pageProps }: AppProps): React.ReactNode => {
    useEffect(() => {
        import('../custom-elements');
    }, []);

    useEffect(() => {
        const mql = window.matchMedia('(prefers-color-scheme: dark)');
        const dark = mql.matches;
        mermaid.initialize({
            darkMode: true,
            theme: dark ? 'dark' : 'default',
        });
    }, []);

    return (
        <>
            <Script strategy="afterInteractive" src={`https://www.googletagmanager.com/gtag/js?id=${gaTrackingID}`} />
            <Script
                id="gtag-init"
                strategy="afterInteractive"
                dangerouslySetInnerHTML={{
                    __html: `
                        window.dataLayer = window.dataLayer || [];
                        function gtag(){dataLayer.push(arguments);}
                        gtag('js', new Date());
                        gtag('config', '${gaTrackingID}', {
                            page_path: window.location.pathname,
                        });
                    `,
                }}
            />
            <Script strategy="afterInteractive" src="https://platform.twitter.com/widgets.js" charSet="utf-8" />
            <Component {...pageProps} />
        </>
    );
};

export default App;
