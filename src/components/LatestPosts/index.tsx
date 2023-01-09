import Link from 'next/link';

import { SidebarContent } from '../SidebarContent';

declare namespace LatestPosts {
    type Props = Readonly<{
        about?: string;
        posts: Post[];
    }>;

    type Post = Readonly<{
        path: string;
        title: string;
    }>;
}

const LatestPosts: React.FC<LatestPosts.Props> = ({ about, posts }) => {
    return (
        <SidebarContent title={`Latest posts${about === undefined ? '' : ` about ${about}`}`}>
            <ul className="space-y-1 pl-2 list-['-_'] list-inside marker:text-gray-500 dark:marker:text-gray-400">
                {posts.map(({ path, title }) => (
                    <li key={path}>
                        <Link href={path} className="hover:text-sky-700 dark:hover:text-amber-500">
                            {title}
                        </Link>
                    </li>
                ))}
            </ul>
        </SidebarContent>
    );
};

export { LatestPosts };
