'use strict';

import Tooltip from 'react-tooltip';

if (typeof window !== 'undefined') {
    window.ReactTooltip = Tooltip;
}

export { Tooltip as reactTooltipCpt };
