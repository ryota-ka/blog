import { SidebarContent } from '..';
import type * as Post from '../../Post';

type Props = {
    className?: string | undefined;
    current: string | null;
    sections: Post.TableOfContents.T;
};

const TableOfContents: React.FC<Props> = ({ className, current, sections }) => {
    const items = sections.flatMap(({ href, subsections, title }) => [
        { title, href, sub: false },
        ...subsections.map(({ href, title }) => ({ href, title, sub: true })),
    ]);

    return (
        <SidebarContent className={className} title="Table of Contents">
            <ul className="space-y-1 pl-1">
                {items.map(({ title, href, sub }) => (
                    <li key={href}>
                        <a
                            href={href}
                            className={
                                'text-gray-600 dark:text-gray-300 block line-clamp-1 hover:text-sky-700 dark:hover:text-amber-500 px-1 rounded-l' +
                                (sub ? ' ml-4' : '') +
                                (current === href
                                    ? ' text-gray-900 bg-gray-300/20 dark:text-white dark:bg-white/20'
                                    : '')
                            }
                        >
                            {title}
                        </a>
                    </li>
                ))}
            </ul>
        </SidebarContent>
    );
};

export { TableOfContents };
