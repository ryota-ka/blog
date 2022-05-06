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
                    <li key={kw}>{kw}</li>
                ))}
            </ul>
        </SidebarContent>
    );
};

export { Keywords };
