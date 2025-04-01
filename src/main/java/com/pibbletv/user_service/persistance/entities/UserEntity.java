package com.pibbletv.user_service.persistance.entities;

import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import jakarta.validation.constraints.NotEmpty;
import org.hibernate.validator.constraints.Length;

@Table("users")  // R2DBC uses this instead of JPA's @Entity
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UserEntity {

    @Id  // R2DBC still uses @Id, but without JPA's @GeneratedValue
    private Long id;

    @NotEmpty(message = "Username is required")
    @Length(min = 3, max = 17, message = "Username must be between 3 and 17 characters")
    @Column("username")  // Use Spring Data @Column
    private String username;

    private byte[] bgImage;  // R2DBC automatically handles byte[] for BLOBs

    private byte[] profileImage;

    @Column("is_banned")
    private Boolean isBanned;
}