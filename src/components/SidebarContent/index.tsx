declare namespace SidebarContent {
    type Props = {
        children: React.ReactNode;
        className?: string | undefined;
        title: string;
    };
}

const SidebarContent: React.FC<SidebarContent.Props> = ({ children, className = '', title }) => {
    return (
        <div
            className={`bg-zinc-50 dark:bg-zinc-900 rounded-xl px-2 py-4 shadow dark:border border-zinc-700 ${className}`}
        >
            <div className="text-lg mb-2 font-medium px-2">{title}</div>
            {children}
        </div>
    );
};

export { SidebarContent };
