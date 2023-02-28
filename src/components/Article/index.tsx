import { forwardRef } from 'react';

declare namespace Article {
    type Props = Readonly<{
        className?: string;
        html: string;
    }>;
}

const Article = forwardRef<HTMLElement, Article.Props>(function Article({ className = '', html }, ref) {
    return <article className={`global-article ${className}`} dangerouslySetInnerHTML={{ __html: html }} ref={ref} />;
});

export { Article };
