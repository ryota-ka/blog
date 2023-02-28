import mermaid from 'mermaid';
import { forwardRef, useEffect } from 'react';

declare namespace Article {
    type Props = Readonly<{
        className?: string;
        html: string;
    }>;
}

const Article = forwardRef<HTMLElement, Article.Props>(function Article({ className = '', html }, ref) {
    useEffect(() => {
        const mql = window.matchMedia('(prefers-color-scheme: dark)');
        const dark = mql.matches;
        mermaid.initialize({
            theme: dark ? 'dark' : 'default',
            startOnLoad: false,
        });
        void mermaid.run({
            querySelector: '.mermaid',
        });
    }, []);

    return <article className={`global-article ${className}`} dangerouslySetInnerHTML={{ __html: html }} ref={ref} />;
});

export { Article };
