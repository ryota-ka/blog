declare namespace Sidebar {
    type Props = {
        children: React.ReactNode;
        className?: string;
    };
}

const Sidebar: React.FC<Sidebar.Props> = ({ children, className = '' }) => (
    <div className={`space-y-4 ${className}`}>{children}</div>
);

export { Sidebar };
