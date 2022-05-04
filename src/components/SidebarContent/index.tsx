declare namespace SidebarContent {
    type Props = {
        children: React.ReactNode;
        title: string;
    };
}

const SidebarContent: React.FC<SidebarContent.Props> = ({ children, title }) => {
    return (
        <div className="bg-zinc-50 dark:bg-zinc-900 rounded-xl px-2 py-4 shadow">
            <div className="text-lg mb-2 font-bold px-2">{title}</div>
            {children}
        </div>
    );
};

export { SidebarContent };
