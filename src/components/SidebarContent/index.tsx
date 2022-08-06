declare namespace SidebarContent {
    type Props = {
        children: React.ReactNode;
        className?: string | undefined;
        title: string;
    };
}

const SidebarContent: React.FC<SidebarContent.Props> = ({ children, className, title }) => {
    return (
        <div className={className}>
            <div className="text-lg mb-2 font-medium px-2">{title}</div>
            {children}
        </div>
    );
};

export { SidebarContent };
