package com.x2dev.reports.bc;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import org.apache.ojb.broker.query.QueryByCriteria;

public class UserAuthTypeResetProcedure extends ProcedureJavaSource {
    private UserDataContainer userData;

    @Override
    protected void execute() throws Exception {
        X2Criteria currentCriteria = getCurrentCriteria();
        QueryByCriteria query = new QueryByCriteria(User.class, currentCriteria);

        try (QueryIterator<X2BaseBean> users = getBroker().getIteratorByQuery(query)) {
            ModelBroker md = new ModelBroker(this.userData);
            while (users.hasNext()) {
                User user = (User) users.next();

                if (!StringUtils.isBlank(user.getIdpGuid())) {
                    user.setIdpGuid(null);
                    user.setAuthenticationType(0);
                    md.saveBeanForced(user);
                }
            }
        }
    }

    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        this.userData = userData;
    }
}
