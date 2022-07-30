import { SidebarContent } from '..';
import type * as Post from '../../Post';

type Props = {
    className?: string | undefined;
    sections: Post.TableOfContents.T;
};

const TableOfContents: React.FC<Props> = ({ className, sections }) => {
    const items = sections.flatMap(({ href, subsections, title }) => [
        { title, href, sub: false },
        ...subsections.map(({ href, title }) => ({ href, title, sub: true })),
    ]);

    return (
        <SidebarContent className={className} title="Table of Contents">
            <ul className="space-y-1 pl-2">
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
        </SidebarContent>
    );
};

export { TableOfContents };
