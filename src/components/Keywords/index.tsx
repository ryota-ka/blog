import Link from 'next/link';

import { SidebarContent } from '..';

declare namespace Keywords {
    type Props = {
        keywords: string[];
    };
}

const Keywords: React.FC<Keywords.Props> = ({ keywords }) => {
    return (
        <SidebarContent title="Keywords">
            <ul className="space-y-1 list-disc list-inside pl-2">
                {keywords.map((kw) => (
                    <li key={kw}>
                        <Link href={`/keywords/${kw}`}>
                            <a className="hover:text-sky-700 dark:hover:text-amber-500">{kw}</a>
                        </Link>
                    </li>
                ))}
            </ul>
        </SidebarContent>
    );
};

export { Keywords };
