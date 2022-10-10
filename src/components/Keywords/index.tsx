import Link from 'next/link';

import { SidebarContent } from '..';

declare namespace Keywords {
    type Props = {
        keywords: Keyword[];
        seeAllKeywords: boolean;
    };

    type Keyword = {
        keyword: string;
        count: number | null;
    };
}

const Keywords: React.FC<Keywords.Props> = ({ keywords, seeAllKeywords }) => {
    return (
        <SidebarContent title="Keywords">
            <div className="ml-2">
                <ul className="space-y-1 mb-2 list-['-_'] list-inside marker:text-gray-500 dark:marker:text-gray-400">
                    {keywords.map(({ count, keyword }) => (
                        <li key={keyword}>
                            <Link href={`/keywords/${keyword}`}>
                                <a className="hover:text-sky-700 dark:hover:text-amber-500">{keyword}</a>
                            </Link>
                            {count !== null && ` (${count})`}
                        </li>
                    ))}
                </ul>
                {seeAllKeywords && (
                    <Link href="/keywords">
                        <a className="hover:text-sky-700 dark:hover:text-amber-500">See all keywords</a>
                    </Link>
                )}
            </div>
        </SidebarContent>
    );
};

export { Keywords };
