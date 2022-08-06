import { SidebarContent } from '..';

declare namespace Links {
    type Props = Record<string, never>;
}

const links = [
    ['ryota-ka.me', 'https://ryota-ka.me/'],
    ['GitHub', 'https://github.com/ryota-ka'],
    ['Twitter', 'https://twitter.com/ryotakameoka'],
];

const Links: React.FC<Links.Props> = () => {
    return (
        <SidebarContent title="Links">
            <ul className="space-y-1 pl-2 list-['-_'] list-inside marker:text-gray-500 dark:marker:text-gray-400">
                {links.map(([title, href]) => (
                    <li key={href}>
                        <a
                            className="hover:text-sky-700 dark:hover:text-amber-500"
                            href={href}
                            target="_blank"
                            rel="noopener noreferrer"
                        >
                            {title}
                        </a>
                    </li>
                ))}
            </ul>
        </SidebarContent>
    );
};

export { Links };
