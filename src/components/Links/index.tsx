import { SidebarContent } from '..';

declare namespace Links {
    type Props = Record<string, never>;
}

const Links: React.FC<Links.Props> = () => {
    return (
        <SidebarContent title="Links">
            <ul className="list-inside list-disc ml-2 space-y-1">
                <li>
                    <a href="https://ryota-ka.me/" target="_blank" rel="noopener noreferrer">
                        ryota-ka.me
                    </a>
                </li>
                <li>
                    <a href="https://github.com/ryota-ka" target="_blank" rel="noopener noreferrer">
                        GitHub
                    </a>
                </li>
                <li>
                    <a href="https://twitter.com/ryotakameoka" target="_blank" rel="noopener noreferrer">
                        Twitter
                    </a>
                </li>
            </ul>
        </SidebarContent>
    );
};

export { Links };
