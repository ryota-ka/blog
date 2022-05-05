import type * as Post from '../../Post';

type Props = {
    className?: string;
    sections: Post.TableOfContents.T;
};

const TableOfContents: React.FC<Props> = ({ className = '', sections }) => {
    const items = sections.flatMap(({ href, subsections, title }) => [
        { title, href, sub: false },
        ...subsections.map(({ href, title }) => ({ href, title, sub: true })),
    ]);

    return (
        <div
            className={`${className} bg-zinc-50 dark:bg-zinc-900 rounded-xl px-2 py-4 shadow dark:border border-zinc-700`}
        >
            <div className="text-lg mb-3 font-bold">Table of Contents</div>
            <ul className="space-y-1">
                {items.map(({ title, href, sub }) => (
                    <li key={href}>
                        <a
                            href={href}
                            className={
                                'text-gray-600 dark:text-gray-300 block line-clamp-1 hover:text-sky-700 dark:hover:text-amber-500 ' +
                                (sub ? 'ml-4' : '')
                            }
                        >
                            {title}
                        </a>
                    </li>
                ))}
            </ul>
        </div>
    );
};

export { TableOfContents };
