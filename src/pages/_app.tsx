import 'katex/dist/katex.css';
import 'highlight.js/styles/github-dark.css';
import '../styles/globals.scss';

import type { AppProps } from 'next/app';
import Script from 'next/script';

declare global {
    // eslint-disable-next-line no-var
    var gtag: any;
}

const gaTrackingID = process.env.NEXT_PUBLIC_GOOGLE_ANALYTICS_TRACKING_ID!;

const App = ({ Component, pageProps }: AppProps): React.ReactNode => {
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
            <Component {...pageProps} />
        </>
    );
};

export default App;
